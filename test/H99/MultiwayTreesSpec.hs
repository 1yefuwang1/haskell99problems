module H99.MultiwayTreesSpec (spec) where

import           H99.MultiwayTrees
import           Test.Hspec

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

tree6 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' [Node 'h' []]]
                ]



spec :: Spec
spec =
  describe "testing multiway trees problems" $ do
    describe "Problem 70C: Count the nodes of a multiway tree" $ do
      it "should work with tree2 and tree1" $ do
        nnodes tree2 `shouldBe` 2
        nnodes tree1 `shouldBe` 1

    describe "Problem 70: Tree construction from a node string" $ do
      describe "testing treeToString" $ do
        it "should work with tree5" $ do
          treeToString tree5 `shouldBe` "afg^^c^bd^e^^^"
          treeToString tree4 `shouldBe` "bd^e^^"

      describe "testing stringToTree" $ do
        it "should work with tree5 string: afg^^c^bd^e^^^" $ do
          stringToTree "afg^^c^bd^e^^^" `shouldBe` tree5
        it "should work with tree6 string: afg^^c^bd^eh^^^^" $ do
          stringToTree "afg^^c^bd^eh^^^^" `shouldBe` tree6
