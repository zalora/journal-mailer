{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Process where


import           Control.Applicative
import           Control.Arrow
import           Control.Monad           ((>=>), join)
import           Data.ByteString         (ByteString)
import           Data.Foldable           (forM_)
import           Data.Function
import           Data.HashMap.Strict     (lookup, toList)
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.String.Conversions
import           Data.Traversable        (sequenceA)
import           Data.Text               (Text)
import           Data.Time.Clock         (UTCTime, addUTCTime)
import           Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import           GHC.IO.Exception        (ExitCode(..))
import           Network.Mail.Mime
import           Pipes
import qualified Pipes.Prelude           as P
import           Prelude                 hiding (lookup)
import           Systemd.Journal
import           System.Process          (readProcessWithExitCode)
import           Text.Read               (readMaybe)

import           Options

process :: (Applicative m, Monad m) => Configuration String -> GetsJournal m -> Pipe JournalFields Mail m ()
process options jget =
  P.map extractPriority >->
  pCatMaybes >->
  P.filter (isSevere . fst) >->
  P.mapM (snd >>> mkMail options jget)

-- * journal stuff

extractPriority :: JournalFields -> Maybe (Priority, JournalFields)
extractPriority fields = do
  p <- lookup "PRIORITY" fields
  (, fields) <$> parsePriority p

parsePriority :: ByteString -> Maybe Priority
parsePriority = readMaybe . cs >=> toEnumMaybe

isSevere :: Priority -> Bool
isSevere p = fromEnum p <= fromEnum Error


type GetsJournal m = MessageSource
                  -> UTCTime        -- start time
                  -> UTCTime        -- end time
                  -> m (Maybe String)


-- | Attempt to get information about context around an entry
mkContextInfo :: (Applicative m, Monad m) => Configuration a
                                          -> JournalFields
                                          -> GetsJournal m
                                          -> m (Maybe String)
mkContextInfo cfg fields gj = join <$> sequenceA (gj source <$> from <*> until)
  where
    source = getMessageSource fields
    until  = utcTime fields
    from   = addUTCTime
         <$> fmap (negate . fromInteger) (contextInterval cfg)
         <*> until

ioJournal :: MonadIO m => GetsJournal m
ioJournal (Unit source) start end = do
  let args = [ "-u", source
             , "--since=" ++ takeWhile (/= '.') (show start)
             , "--until=" ++ takeWhile (/= '.') (show end)
             , "--no-pager"
             ]
  (e, stdout, _) <- liftIO $ readProcessWithExitCode "journalctl" args ""
  case e of
    ExitSuccess -> return $ Just stdout
    _           -> return Nothing
ioJournal _ _ _ = return Nothing

-- * mail stuff

mkMail :: (Applicative m, Monad m) => Configuration String
                                   -> GetsJournal m
                                   -> JournalFields
                                   -> m Mail
mkMail options jget fields = do
  ctx <- mkContextInfo options fields jget
  let cts = fromMaybe "<no context available>" ctx
  return (addPart [plainPart $ cs (date ++ pretty fields ++ "\n" ++ cts)] $
                   mailFromToSubject
                     (addr (sender options))
                     (map addr mailReceivers)
                     subject)
 where
  messageSource = getMessageSource fields
  mailReceivers = lookupReceivers (messageSourceName messageSource) options
  date = "On " <> maybe "<unknown date>" show (utcTime fields) <> ":\n\n"
  subject = "error message on " <> host fields <> ": "
         <> showMessageSource messageSource <> outcome fields

  addr :: String -> Address
  addr = Address Nothing . cs

  pretty :: JournalFields -> String
  pretty =
    toList >>>
    map (\ (key, value) -> prettyJournalField key ++ " = " ++ cs value) >>>
    unlines

mailFromToSubject :: Address -> [Address] -> Text -> Mail
mailFromToSubject from to subject = (emptyMail from){
  mailTo = to,
  mailHeaders = [("Subject", subject)]
 }

host :: JournalFields -> Text
host = maybe "<unknown host>" cs . lookup "_HOSTNAME"

utcTime :: JournalFields -> Maybe UTCTime
utcTime jf = do
        let toSec y = y `div` 10 ^ (6::Int)
        msrt <- lookup "_SOURCE_REALTIME_TIMESTAMP" jf
        micros <- readMaybe $ cs msrt
        return . posixSecondsToUTCTime . fromInteger . toSec $ micros

data MessageSource
  = Unit String
  | UnknownType String
  | UnknownSource

messageSourceName :: MessageSource -> Maybe String
messageSourceName (Unit name) = Just name
messageSourceName (UnknownType name) = Just name
messageSourceName UnknownSource = Nothing

showMessageSource :: MessageSource -> Text
showMessageSource (Unit name) = "systemd unit " <> cs (addQuotes name)
showMessageSource (UnknownType name) = cs name
showMessageSource UnknownSource = "<unknown message source>"

getMessageSource :: JournalFields -> MessageSource
getMessageSource fields = fromMaybe UnknownSource $
  ((Unit . cs <$> lookup "UNIT" fields) <|>
   (UnknownType . cs <$> lookup "_COMM" fields) <|>
   (UnknownType . cs <$> lookup "SYSLOG_IDENTIFIER" fields))

-- | combines RESULT and MESSAGE (if they exist)
outcome :: JournalFields -> Text
outcome fields = cs $ case (lookup "RESULT" fields, lookup "MESSAGE" fields) of
  (Just result, Just message) -> ": " <> result <> ": " <> message
  (Just result, Nothing) -> ": " <> result
  (Nothing, Just message) -> ": " <> message
  (Nothing, Nothing) -> ""

prettyJournalField :: JournalField -> String
prettyJournalField =
  show >>>
  drop (length ("JournalField " :: String)) >>>
  read


-- * utils

pCatMaybes :: Monad m => Pipe (Maybe a) a m ()
pCatMaybes = do
  mx <- await
  forM_ mx yield
  pCatMaybes

toEnumMaybe :: (Enum a, Bounded a) => Int -> Maybe a
toEnumMaybe n
  | n >= minBound && n <= maxBound = Just $ toEnum n
  | otherwise = Nothing

addQuotes :: (IsString a, Monoid a) => a -> a
addQuotes t = "\"" <> t <> "\""
