{-# LANGUAGE OverloadedStrings, TupleSections #-}


module Run where

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (forM_)
import Data.ByteString (ByteString)
import Data.Foldable (forM_)
import Data.Function
import Data.HashMap.Strict (lookup, toList)
import Data.String.Conversions
import Data.Text (Text)
import Network.Mail.Mime
import Pipes
import Pipes.Safe
import Prelude hiding (lookup)
import qualified Pipes.Prelude as P
import Systemd.Journal
import System.Environment
import System.Exit.Compat
import Text.Read (readMaybe)


run :: IO ()
run = do
    receivers <- getArgs
    case receivers of
        [] -> do
            progName <- getProgName
            die ("usage: " ++ progName ++ " EMAIL_ADDRESS...")
        _ -> runEffect $ runSafeP $ effect receivers


effect :: [String] -> Effect (SafeT IO) ()
effect receivers =
    journal >->
    P.map extractPriority >->
    pCatMaybes >->
    P.filter (isSevere . fst) >->
    for cat (liftIO . notify receivers . snd)


journal :: MonadSafe m => Producer JournalEntry m ()
journal = openJournal [] FromEnd Nothing Nothing


extractPriority :: JournalEntry -> Maybe (Priority, JournalEntry)
extractPriority entry = do
    p <- lookup "PRIORITY" (journalEntryFields entry)
    (, entry) <$> parsePriority p

parsePriority :: ByteString -> Maybe Priority
parsePriority = readMaybe . cs >=> toEnumMaybe

isSevere :: Priority -> Bool
isSevere p = fromEnum p <= fromEnum Error


notify :: [String] -> JournalEntry -> IO ()
notify receivers entry =
    renderSendMail $
        addPart [plainPart $ cs (pretty entry)] $
        mailFromToSubject
            (addr "datascience@zalora.com")
            (map addr receivers)
            ("epsilon: systemd unit " <> unitName entry <> " failed")
  where
    addr :: String -> Address
    addr = Address Nothing . cs

    pretty :: JournalEntry -> String
    pretty =
        journalEntryFields >>>
        toList >>>
        map (\ (key, value) -> prettyJournalField key ++ " = " ++ cs value) >>>
        unlines

unitName :: JournalEntry -> Text
unitName = maybe "<unknown>" (cs . show) . lookup "UNIT" . journalEntryFields

prettyJournalField :: JournalField -> String
prettyJournalField =
    show >>>
    drop (length ("JournalField " :: String)) >>>
    read


-- utils

mailFromToSubject :: Address -> [Address] -> Text -> Mail
mailFromToSubject from to subject = (emptyMail from) 
  { mailTo = to
  , mailHeaders = [("Subject", subject)]
  }

pCatMaybes :: Monad m => Pipe (Maybe a) a m ()
pCatMaybes = do
    mx <- await
    forM_ mx yield
    pCatMaybes

toEnumMaybe :: (Enum a, Bounded a) => Int -> Maybe a
toEnumMaybe n
    | n >= minBound && n <= maxBound = Just $ toEnum n
    | otherwise = Nothing
