module H99.MiscellaneousSpec (spec) where

import           Data.List         (foldl')
import           H99.Miscellaneous
import           Test.Hspec

spec :: Spec
spec = describe "Testing Miscellaneous Problems: " $ do
  context "Problem 90: 8 queens" $ do
    it "should work with 8 queens" $ do
      let result = queens 8
      length result `shouldBe` 92
      head result `shouldBe` [1,5,8,6,3,7,2,4]

    it "should return [] when called on 2, 3" $
      fmap queens [2, 3] `shouldBe` [[], []]

  context "Problem 91: Knight's tour(finds all solutions)" $ do
    it "should return [] with size 4" $
      knightsTour 4 `shouldBe` []

    it "should work with size 30" $ do
      let
        check xs = length xs == 30 * 30 &&
            fst (foldl' (\(isValid, l) r -> (isValid && (r `elem` nextSteps l), r)) (True, head xs) (tail xs))
      check (head (knightsTour 30)) `shouldBe` True
