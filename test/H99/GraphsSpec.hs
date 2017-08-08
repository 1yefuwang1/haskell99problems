module H99.GraphsSpec (spec) where

import qualified Data.Set          as Set
import           H99.Graphs
import           H99.MultiwayTrees as T
import           Test.Hspec

graph1 = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
g = Graph [1,2,3,4,5,6] [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

spec :: Spec
spec =
  describe "Graph Tests" $ do
    describe "Problem 80: Conversions" $ do
      it "should convert graph term to Adj term" $
        graphToAdj graph1 `shouldBe`
          Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]


    describe "Problem 81: Path from one node to another one" $ do
      it "should produce [[1,2,3,4], [1,3,4]] when runing with `from 1 to 4`" $
        Set.fromList (paths 1 4 g) `shouldBe` Set.fromList [[1,2,3,4], [1,3,4], [1,2,4], [1,3,2,4]]
      it "should produce [] when ruing with `from 2 to 6`" $
        paths 1 6 g `shouldBe` []

    describe "Problem 82: Cycle from a given node" $ do
      it "should work when running with 2" $
        cycle' 2 g `shouldBe` [[2,1,3,2],[2,1,3,4,2],[2,3,1,2],[2,3,4,2],[2,4,3,2],[2,4,3,1,2]]

      it "should work when running with 5" $
        cycle' 5 g `shouldBe` []

    describe "Problem 83: Construct all spanning trees" $ do
      let g1 = Graph [1, 2] [(1, 2)]
      let g2 = Graph [1, 2, 3] [(1, 2), (2, 3), (3, 1)]
      let tree1 = Graph [1, 2, 3] [(1, 2), (1, 3)]
      it "should return [] when called with g" $
        spantree g `shouldBe` []

      it "should work with g1" $
        spantree g1 `shouldBe` [Graph [1, 2] [(1, 2)]]

      it "should work with g2" $
        spantree g2 `shouldBe` [Graph [1,2,3] [(3,2),(2,1)],Graph [3,2,1] [(3,1),(2,1)],Graph [2,3,1] [(2,3),(3,1)]]
