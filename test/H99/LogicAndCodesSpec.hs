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

    it "Problem 48: print truth table with multiple arguments" $ do
      tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c) `shouldBe` [[True, True, True, True], [True, True, False, True], [True, False, True, True], [True, False, False, True], [False, True, True, True], [False, True, False, True], [False, False, True, True], [False, False, False, True]]

    it "Problem 49: gray codes" $ do
      gray 3 `shouldBe` ["000","001","011","010","110","111","101","100"]

    it "Problem 50: huffman" $ do
      huffman [("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)] `shouldBe`
        [("a", "0"), ("b", "101"), ("c", "100"), ("d",  "111"), ("e", "1101"), ("f", "1100")]


