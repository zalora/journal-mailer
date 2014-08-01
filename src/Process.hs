{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Process where


import           Control.Applicative
import           Control.Arrow
import           Control.Monad           hiding (forM_)
import           Data.ByteString         (ByteString)
import           Data.Foldable           (forM_)
import           Data.Function
import           Data.HashMap.Strict     (lookup, toList)
import           Data.String.Conversions
import           Data.Text               (Text)
import           Network.Mail.Mime
import           Pipes
import qualified Pipes.Prelude           as P
import           Prelude                 hiding (lookup)
import           Systemd.Journal
import           Text.Read               (readMaybe)


process :: Monad m => [String] -> Pipe JournalEntry Mail m ()
process receivers =
  P.map extractPriority >->
  pCatMaybes >->
  P.filter (isSevere . fst) >->
  P.map (snd >>> mkMail receivers)


-- * journal stuff

extractPriority :: JournalEntry -> Maybe (Priority, JournalEntry)
extractPriority entry = do
  p <- lookup "PRIORITY" (journalEntryFields entry)
  (, entry) <$> parsePriority p

parsePriority :: ByteString -> Maybe Priority
parsePriority = readMaybe . cs >=> toEnumMaybe

isSevere :: Priority -> Bool
isSevere p = fromEnum p <= fromEnum Error


-- * mail stuff

mkMail :: [String] -> JournalEntry -> Mail
mkMail receivers entry =
  addPart [plainPart $ cs (pretty entry)] $
  mailFromToSubject
    (addr "devops@zalora.com")
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

mailFromToSubject :: Address -> [Address] -> Text -> Mail
mailFromToSubject from to subject = (emptyMail from){
  mailTo = to,
  mailHeaders = [("Subject", subject)]
 }

unitName :: JournalEntry -> Text
unitName = maybe "<unknown>" (cs . show) . lookup "UNIT" . journalEntryFields

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
