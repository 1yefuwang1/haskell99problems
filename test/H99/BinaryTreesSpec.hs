module H99.BinaryTreesSpec (spec) where

import           Data.Set        as S
import           H99.BinaryTrees
import           Test.Hspec

spec :: Spec
spec =
  describe "Binary Trees tests" $ do
    it "Problem 54: not for haskell" $ do
      1 `shouldBe` 1

    it "Problem 55: cbalTree" $ do
      cbalTree 0 `shouldBe` [Empty]
      cbalTree 1 `shouldBe` [leaf 'x']
      cbalTree 4 `shouldBe`
        [Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty),
          Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
          Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty),
          Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]

    it "Problem 56: symmetric" $ do
      symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) `shouldBe` False
      symmetric (Empty :: Tree Int) `shouldBe` True
      symmetric (Branch 'x' Empty Empty) `shouldBe` True
      symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` True

    it "Problem 57: Binary seach trees" $ do
      (symmetric . construct) [5, 3, 18, 1, 4, 12, 21] `shouldBe` True
      (symmetric . construct) [3, 2, 5, 7, 1] `shouldBe` True

    it "Problem 58: symCbalTrees" $ do
      S.fromList (symCbalTrees 5) `shouldBe`
        S.fromList [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]

    it "Problem 59: hbalTree" $ do
      hbalTree 0 `shouldBe` [(Empty :: Tree Char)]
      length (hbalTree 3) `shouldBe` 15

    it "Problem 60: hbalTreeNodes" $ do
      length (hbalTreeNodes 15) `shouldBe` 1553
