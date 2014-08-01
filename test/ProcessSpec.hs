{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProcessSpec where


import           Data.ByteString         (ByteString)
import           Data.Char
import           Data.Foldable
import           Data.HashMap.Strict     (fromList)
import           Data.String
import           Data.String.Conversions
import           Network.Mail.Mime
import           Pipes
import qualified Pipes.Prelude           as P
import           Systemd.Journal
import           Test.Hspec
import           Test.QuickCheck

import           Process


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "prettyJournalField" $ do
    it "converts journal fields to strings" $ do
      prettyJournalField (fromString "foo") `shouldBe` "FOO"

    it "converts arbitrary fields to strings" $ do
      property $ \ s ->
        prettyJournalField (fromString s) `shouldBe` map toUpper s

  describe "process" $ do
    it "sends out mails for entries with priority 3" $ do
      let mails = process' ["rec"] [("PRIORITY", "3")]
      mails `shouldSatisfy` (not . null)

    it "includes the hostname in the subject" $ do
      let mails = process' ["rec"] $
            ("PRIORITY", "3") :
            ("_HOSTNAME", "host_foo_bar") :
            []
      forM_ mails $ \ mail ->
        getSubject mail `shouldBe` "host_foo_bar: systemd unit <unknown> failed"


deriving instance Show Mail
deriving instance Show Address
deriving instance Show Part
deriving instance Show Encoding

process' :: [String] -> [(JournalField, ByteString)] -> [Mail]
process' receivers fields =
  P.toList (yield (fromList fields) >-> process receivers)

getSubject :: Mail -> String
getSubject mail =
  maybe (error "header 'Subject' not found") cs $
  lookup "Subject" (mailHeaders mail)
