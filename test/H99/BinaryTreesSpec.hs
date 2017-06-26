module H99.BinaryTreesSpec (spec) where

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

