{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProcessSpec where


import           Data.ByteString         (ByteString)
import           Data.Char
import           Data.Foldable
import           Data.HashMap.Strict     (fromList)
import           Data.List
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
      let mails = process' [("PRIORITY", "3")]
      mails `shouldSatisfy` (not . null)

    it "includes the hostname in the subject" $ do
      let mails = process' $
            ("PRIORITY", "3") :
            ("_HOSTNAME", "host_foo_bar") :
            []
      forM_ mails $ \ mail ->
        getSubject mail `shouldContain` "host_foo_bar"

    it "reports a failing unit when it sees one" $ do
      let mails = process' $
            ("PRIORITY", "3") :
            ("_HOSTNAME", "host_foo_baz") :
            ("UNIT", "unit_bar") :
            []
      forM_ mails $ \ mail ->
        getSubject mail `shouldContain` "systemd unit \"unit_bar\""

    it "includes the RESULT field in the subject" $ do
      let mails = process' $
            ("PRIORITY", "3") :
            ("RESULT", "bla_result") :
            []
      forM_ mails $ \ mail ->
        getSubject mail `shouldContain` "bla_result"

    it "includes the _COMM field in the subject" $ do
      let mails = process' $
            ("PRIORITY", "3") :
            ("_COMM", "sshd") :
            []
      forM_ mails $ \ mail ->
        getSubject mail `shouldSatisfy` ("sshd" `isInfixOf`)

    it "includes the MESSAGE field in the subject" $ do
      let mails = process' $
            ("PRIORITY", "3") :
            ("MESSAGE", "bla_message") :
            []
      forM_ mails $ \ mail ->
        getSubject mail `shouldContain` "bla_message"

    it "includes entry fields in the body" $ do
      let mails = process' $
            ("PRIORITY", "3") :
            ("SOME_FIELD", "some_value") :
            []
      forM_ mails $ \ mail -> do
        getBody mail `shouldContain` "SOME_FIELD"
        getBody mail `shouldContain` "some_value"


deriving instance Show Mail
deriving instance Show Address
deriving instance Show Part
deriving instance Show Encoding

process' :: [(JournalField, ByteString)] -> [Mail]
process' fields =
  P.toList (yield (fromList fields) >-> process receivers)
 where
  receivers = ["fakeReceivers"]

getSubject :: Mail -> String
getSubject mail =
  maybe (error "header 'Subject' not found") cs $
  lookup "Subject" (mailHeaders mail)

getBody :: Mail -> String
getBody mail = case mailParts mail of
  [[body]] -> cs $ partContent body
  _ -> error "cannot extract body from parts in mail: " ++ show (mailParts mail)
