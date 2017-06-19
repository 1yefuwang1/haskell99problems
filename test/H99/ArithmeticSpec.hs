module H99.ArithmeticSpec (spec) where

import           H99.Arithmetic
import           Test.Hspec

spec :: Spec
spec =
  describe "testing Arithmetic problems" $ do
    it "Problem 31: isPrime" $ do
      (map isPrime ([2, 3, 4, 5, 6] :: [Int])) `shouldBe` [True, True, False, True, False]

    it "Problem 32: myGCD" $ do
      [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] `shouldBe` [9,3,3]

    it "Problem 33: coprime" $ do
      coprime 2 3 `shouldBe` True
      coprime 2 4 `shouldBe` False

    it "Problem 34: totient" $ do
      map totient [2, 10] `shouldBe` [1, 4]

    it "Problem 35: primeFactors" $ do
      primeFactors 2 `shouldBe` [2]
      primeFactors 315 `shouldBe` [3, 3, 5, 7]
