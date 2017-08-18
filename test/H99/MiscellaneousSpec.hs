module H99.MiscellaneousSpec (spec) where

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

