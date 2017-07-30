module H99.GraphsSpec (spec) where

import qualified Data.Set   as Set
import           H99.Graphs
import           Test.Hspec

graph1 = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

spec :: Spec
spec =
  describe "Graph Tests" $ do
    describe "Problem 80: Conversions" $ do
      it "should convert graph term to Adj term" $ do
        graphToAdj graph1 `shouldBe`
          Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]


    describe "Problem 81: Path from one node to another one" $ do
      let g = Graph [1,2,3,4,5,6] [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
      it "should produce [[1,2,3,4], [1,3,4]] when runing with `from 1 to 4`" $ do
        Set.fromList (paths 1 4 g) `shouldBe` Set.fromList [[1,2,3,4], [1,3,4], [1,2,4], [1,3,2,4]]
      it "should produce [] when ruing with `from 2 to 6`" $ do
        paths 1 6 g `shouldBe` []

