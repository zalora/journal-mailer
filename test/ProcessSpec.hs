
module ProcessSpec where

import Data.Char
import Data.String
import Test.Hspec
import Test.QuickCheck

import Process


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
