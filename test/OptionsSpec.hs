
module OptionsSpec where


import           System.Environment
import           Test.Hspec

import           Options


spec :: Spec
spec = do
  describe "getConfiguration" $ do

    it "uses (non-option) arguments as receiver addresses" $ do
      options <- withArgs ["foo@bar.com", "baz@bar.com", "--sender", "bla"] $ getConfiguration
      receivers options `shouldBe` ["foo@bar.com", "baz@bar.com"]

    it "understands --sender" $ do
      options <- withArgs ["foo@bar.com", "--sender", "sender@foo.com"] $ getConfiguration
      sender options `shouldBe` "sender@foo.com"
