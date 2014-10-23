{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ProcessSpec where


import           Data.ByteString         (ByteString)
import           Data.Char
import           Data.Foldable           (toList)
import           Data.HashMap.Strict     as HashMap (empty, fromList, lookup)
import           Data.List               as List (isInfixOf, lookup)
import           Data.Maybe
import           Data.String
import           Data.String.Conversions
import           Network.Mail.Mime
import           Pipes
import qualified Pipes.Prelude           as P
import           Systemd.Journal
import           Test.Hspec
import           Test.QuickCheck

import           Options
import           Process


main :: IO ()
main = hspec spec

options :: Configuration String
options = Configuration {
  showHelp = False,
  sender = "foo@bar",
  receivers = ["fakeReceiver@bar"],
  receiverMap = empty
 }

process' :: [(JournalField, ByteString)] -> Mail
process' = process'' options

process'' :: Configuration String -> [(JournalField, ByteString)] -> Mail
process'' config fields =
  case P.toList (yield (HashMap.fromList fields) >-> process config) of
    [mail] -> mail
    [] -> error "process'': no mail"
    _ -> error "process'': more than one mail"

spec :: Spec
spec = do
  describe "prettyJournalField" $ do
    it "converts journal fields to strings" $ do
      prettyJournalField (fromString "foo") `shouldBe` "FOO"

    it "converts arbitrary fields to strings" $ do
      property $ \ s ->
        prettyJournalField (fromString s) `shouldBe` map toUpper s

  describe "process" $ do
    it "uses the sender address provided on the command line" $ do
      let mail = process' [("PRIORITY", "3")]
      getSender mail `shouldBe` "foo@bar"

    it "includes the hostname in the subject" $ do
      let mail = process' $
            ("PRIORITY", "3") :
            ("_HOSTNAME", "host_foo_bar") :
            []
      getSubject mail `shouldContain` "host_foo_bar"

    it "reports a failing unit when it sees one" $ do
      let mail = process' $
            ("PRIORITY", "3") :
            ("_HOSTNAME", "host_foo_baz") :
            ("UNIT", "unit_bar") :
            []
      getSubject mail `shouldContain` "systemd unit \"unit_bar\""

    it "includes the RESULT field in the subject" $ do
      let mail = process' $
            ("PRIORITY", "3") :
            ("RESULT", "bla_result") :
            []
      getSubject mail `shouldContain` "bla_result"

    it "includes the _COMM field in the subject" $ do
      let mail = process' $
            ("PRIORITY", "3") :
            ("_COMM", "sshd") :
            []
      getSubject mail `shouldSatisfy` ("sshd" `isInfixOf`)

    it "includes the parsed _SOURCE_REALTIME_TIMESTAMP field in the body" $ do
      let mail = process' $
            ("PRIORITY", "3") :
            ("_SOURCE_REALTIME_TIMESTAMP", "1413551550000001") :
            []
      getBody mail `shouldContain` "2014-10-17 13:12:30 UTC"

    it "includes the MESSAGE field in the subject" $ do
      let mail = process' $
            ("PRIORITY", "3") :
            ("MESSAGE", "bla_message") :
            []
      getSubject mail `shouldContain` "bla_message"

    it "includes SYSLOG_IDENTIFIER (as messages source) when nothing else is present" $ do
      let mail = process' $
            ("PRIORITY", "3") :
            ("SYSLOG_IDENTIFIER", "foo_syslog_identifier") :
            []
      getSubject mail `shouldContain` "foo_syslog_identifier"

    it "includes entry fields in the body" $ do
      let mail = process' $
            ("PRIORITY", "3") :
            ("SOME_FIELD", "some_value") :
            []
      getBody mail `shouldContain` "SOME_FIELD"
      getBody mail `shouldContain` "some_value"


    let unitKeys = ["SYSLOG_IDENTIFIER", "_COMM", "UNIT"]
        configWithReceiverMap = options{
          receiverMap = fromList $
            ("unit1", ["rec1", "rec2"]) :
            ("unit2", ["rec3"]) :
            []
         }
        interested :: String -> [String]
        interested unitName = fromMaybe [] (HashMap.lookup unitName (receiverMap configWithReceiverMap))
        notInterested :: String -> [String]
        notInterested unitName = filter (not . (`elem` interested unitName)) $
          concat (toList (receiverMap configWithReceiverMap))

    it "sends every notification to the standard receiver" $
      property $
      forAll (elements unitKeys) $ \ unitKey ->
      forAll (elements ["unitName_foo", "unit1", "unit2"]) $ \ unitName ->
        let mail = process'' configWithReceiverMap $
              ("PRIORITY", "3") :
              (unitKey, unitName) :
              []
        in "fakeReceiver@bar" `elem` getReceivers mail

    it "sends mails to receivers specified to certain message sources" $
      property $
      forAll (elements unitKeys) $ \ unitKey ->
      forAll (elements ["unit1", "unit2"]) $ \ unitName ->
      forAll (elements (interested unitName)) $
        \ receiver ->
          let mail = process'' configWithReceiverMap $
                ("PRIORITY", "3") :
                (unitKey, cs unitName) :
                []
          in counterexample (show mail) $
             counterexample (show (receiverMap configWithReceiverMap)) $
             receiver `elem` getReceivers mail

    it "does not send mails to receivers for different message sources" $
      property $
      forAll (elements unitKeys) $ \ unitKey ->
      forAll (elements ["unitName_foo", "unit1", "unit2"]) $ \ unitName ->
      forAll (elements (notInterested unitName)) $
        \ receiver ->
          let mail = process'' configWithReceiverMap $
                ("PRIORITY", "3") :
                (unitKey, cs unitName) :
                []
          in not (receiver `elem` getReceivers mail)

    it "does send mails to people interested in 'UNIT_NAME', when receiving \
       \messages from 'UNIT_NAME.service'" $
      property $
      forAll (elements unitKeys) $ \ unitKey ->
      forAll (elements ["unit1", "unit2"]) $ \ unitName ->
      forAll (elements (interested unitName)) $
        \ receiver ->
          let mail = process'' configWithReceiverMap $
                ("PRIORITY", "3") :
                (unitKey, cs unitName <> ".service") :
                []
          in counterexample (show mail) $
             counterexample (show (receiverMap configWithReceiverMap)) $
             receiver `elem` getReceivers mail


deriving instance Show Mail
deriving instance Show Address
deriving instance Show Part
deriving instance Show Encoding

getSender :: Mail -> String
getSender = cs . addressEmail . mailFrom

getReceivers :: Mail -> [String]
getReceivers = map (cs . addressEmail) . mailTo

getSubject :: Mail -> String
getSubject mail =
  maybe (error "header 'Subject' not found") cs $
  List.lookup "Subject" (mailHeaders mail)

getBody :: Mail -> String
getBody mail = case mailParts mail of
  [[body]] -> cs $ partContent body
  _ -> error "cannot extract body from parts in mail: " ++ show (mailParts mail)
