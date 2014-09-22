
module OptionsSpec where


import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Silently
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
