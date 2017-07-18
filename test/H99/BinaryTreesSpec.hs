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
      hbalTree 0 `shouldBe` [Empty :: Tree Char]
      length (hbalTree 3) `shouldBe` 15

    it "Problem 60: hbalTreeNodes" $ do
      length (hbalTreeNodes 15) `shouldBe` 1553

    it "Problem 61: countLeaves" $ do
      countLeaves (Empty :: Tree Char) `shouldBe` 0
      countLeaves (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) `shouldBe` 2

    it "Problem 61A: leaves" $ do
      leaves (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) `shouldBe` [4, 2]

    it "Problem 62: internals" $ do
      internals (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) `shouldBe` [1, 2]

    it "Problem 62B: atLevel" $ do
      atLevel (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) 2 `shouldBe` [2, 2]

    it "Problem 63: completeBinaryTree" $ do
      completeBinaryTree 4 `shouldBe` Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
      isCompleteBinaryTree (completeBinaryTree 4) `shouldBe` True
      isCompleteBinaryTree (hbalTree 5 !! 2) `shouldBe` False

    it "Problem 64: layout" $ do
      layout (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) `shouldBe` Branch {value = (1,(3,1)), left = Branch {value = (2,(1,2)), left = Empty, right = Branch {value = (4,(2,3)), left = Empty, right = Empty}}, right = Branch {value = (2,(4,2)), left = Empty, right = Empty}}

    it "Probelm 65: layout'" $ do
      layout' (Branch 1 (Branch 2 (Branch 3 Empty Empty) (Branch 4 Empty Empty)) (Branch 2 Empty Empty))
        `shouldBe` Branch {value = (1,(4,1)), left = Branch {value = (2,(2,2)), left = Branch {value = (3,(1,3)), left = Empty, right = Empty}, right = Branch {value = (4,(3,3)), left = Empty, right = Empty}}, right = Branch {value = (2,(6,2)), left = Empty, right = Empty}}

    it "Problem 66: layout'': not implemented for now" $ do
      1 `shouldBe` 1

    describe "Problem 67A: a string representation of binary trees" $ do
      it "treeToString" $ do
        treeToString (Empty :: Tree Char) `shouldBe` ""
        treeToString
          (Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty)))
            `shouldBe` "x(y,a(,b))"

      it "stringToTree" $ do
        stringToTree "" `shouldBe` Just (Empty :: Tree Char)
        stringToTree "x(y,a(,b))" `shouldBe`
          Just (Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty)))

    it "Problem 68: preorder and inorder sequences" $ do
      let Just t = stringToTree "a(b(d,e),c)"
          po = preorder t
          io = inorder t
      preInTree po io `shouldBe` t



