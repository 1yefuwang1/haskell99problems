module Haskell99Problems.ArithmeticSpec (spec) where

import           Haskell99Problems.Arithmetic
import           Test.Hspec

spec :: Spec
spec =
  describe "testing Arithmetic problems" $ do
    it "Problem 31: isPrime" $ do
      (map isPrime ([2, 3, 4, 5, 6] :: [Int])) `shouldBe` [True, True, False, True, False]

    it "Problem 32: myGCD" $ do
      [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] `shouldBe` [9,3,3]

