module H99.ArithmeticSpec (spec) where

import           H99.Arithmetic
import           Test.Hspec

spec :: Spec
spec =
  describe "Problem 31: isPrime" $ do
    it "should work" $ do
      (map isPrime ([2, 3, 4, 5, 6] :: [Int])) `shouldBe` [True, True, False, True, False]
