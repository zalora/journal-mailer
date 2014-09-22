
module OptionsSpec where


import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
import           System.IO.Temp
import           Test.Hspec

import           Options


spec :: Spec
spec = do
  describe "getConfiguration" $ do

    it "understands --receiver" $ do
      options <- withArgs [
        "--receiver", "foo@bar.com",
        "--sender", "bla"] $
          getConfiguration
      receivers options `shouldBe` ["foo@bar.com"]

    it "allows to specify multiple receivers" $ do
      options <- withArgs [
        "--receiver", "foo@bar.com",
        "--receiver", "baz@bar.com",
        "--sender", "bla"] $
          getConfiguration
      receivers options `shouldBe` ["foo@bar.com", "baz@bar.com"]

    it "understands --sender" $ do
      options <- withArgs [
        "--receiver", "foo@bar.com",
        "--sender", "sender@foo.com"] $
          getConfiguration
      sender options `shouldBe` "sender@foo.com"

    it "uses the last specified sender" $ do
      options <- withArgs [
        "--receiver", "foo@bar.com",
        "--sender", "sender1@foo.com",
        "--sender", "sender2@foo.com"] $
          getConfiguration
      sender options `shouldBe` "sender2@foo.com"

    it "raises an error when given useless arguments" $ do
      output <- hCapture_ [stderr] $ withArgs ["bla"] $
        (getConfiguration `shouldThrow` (/= ExitSuccess))
      lines output `shouldContain` ["unused argument: bla"]

    let configFileContents :: String
        configFileContents = "\
          \sender: sender@example.com\n\
          \receivers:\n\
          \  - receiver1@example.com\n\
          \  - receiver2@example.com\n"

    it "reads a configuration from a config file specified with --config" $ do
      withSystemTempFile "journal-mailer-test-suite" $ \ configFile handle -> do
        hPutStr handle configFileContents
        hClose handle
        options <- withArgs ["--config", configFile] $ getConfiguration
        options `shouldBe` Configuration {
          showHelp = False,
          sender = "sender@example.com",
          receivers =
            "receiver1@example.com" :
            "receiver2@example.com" :
            []
         }

    it "parses the example configuration successfully" $ do
      _ <- withArgs ["--config", "journal-mailer.config.example"] $ getConfiguration
      return ()
