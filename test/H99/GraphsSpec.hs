module H99.GraphsSpec (spec) where

import           H99.Graphs
import           Test.Hspec

graph1 = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

spec :: Spec
spec =
  describe "Problem 80: Conversions" $ do
    it "should convert graph term to Adj term" $ do
      graphToAdj graph1 `shouldBe`
        Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]

