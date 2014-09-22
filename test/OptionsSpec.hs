
module OptionsSpec where


import           System.Environment
import           Test.Hspec

import           Options


spec :: Spec
spec = do
  describe "getOptions" $ do

    it "uses (non-option) arguments as receiver addresses" $ do
      options <- withArgs ["foo@bar.com", "baz@bar.com", "--sender", "bla"] $ getOptions
      receivers options `shouldBe` ["foo@bar.com", "baz@bar.com"]

    it "understands --sender" $ do
      options <- withArgs ["foo@bar.com", "--sender", "sender@foo.com"] $ getOptions
      sender options `shouldBe` "sender@foo.com"
