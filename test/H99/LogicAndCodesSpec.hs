module H99.LogicAndCodesSpec (spec) where

import           H99.LogicAndCodes
import           Test.Hspec

spec :: Spec
spec =
  describe "testing logic and codes problems" $ do
    it "Problem 46: print truth table" $ do
      table (\a b -> (and' a (or' a b))) `shouldBe` [(True, True, True), (True, False, True), (False, True, False), (False, False, False)]

    it "Problem 47: print truth table continued" $ do
      table' (\a b -> (a `and'` (a `or'` b))) `shouldBe` [(True, True, True), (True, False, True), (False, True, False), (False, False, False)]


