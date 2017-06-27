module H99.BinaryTrees where

import           Control.Monad (replicateM)
import           Data.List     (foldl')

data Tree a = Empty
            | Branch {value :: a, left :: Tree a, right :: Tree a}
            deriving (Show, Eq)

leaf x = Branch x Empty Empty

{-
Problem 54A
(*) Check whether a given term represents a binary tree

In Prolog or Lisp, one writes a predicate to do this.

Example in Lisp:

* (istree (a (b nil nil) nil))
T
* (istree (a (b nil nil)))
NIL
Non-solution:

Haskell's type system ensures that all terms of type Tree a are binary trees: it is just not possible to construct an invalid tree with this type. Hence, it is redundant to introduce a predicate to check this property: it would always return True.

-}

{-
Problem 55
(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.

Example:

* cbal-tree(4,T).
T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
etc......No
Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:

*Main> cbalTree 4
[
-- permutation 1
--     x
--    / \
--   x   x
--        \
--         x
Branch 'x' (Branch 'x' Empty Empty)
           (Branch 'x' Empty
                       (Branch 'x' Empty Empty)),

-- permutation 2
--     x
--    / \
--   x   x
--      /
--     x
Branch 'x' (Branch 'x' Empty Empty)
           (Branch 'x' (Branch 'x' Empty Empty)
                       Empty),

-- permutation 3
--     x
--    / \
--   x   x
--    \
--     x
Branch 'x' (Branch 'x' Empty
                       (Branch 'x' Empty Empty))
           (Branch 'x' Empty Empty),

-- permutation 4
--     x
--    / \
--   x   x
--  /
-- x
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty)
                       Empty)
           (Branch 'x' Empty Empty)
]
-}

cbalTree :: Int -> [Tree Char]
cbalTree 0 = return $ Empty
cbalTree 1 = return $ leaf 'x'
cbalTree n =
  if odd n
    then do
      [l, r] <- replicateM 2 $ cbalTree $ half
      return $ Branch 'x' l r
    else concat $ do
      l <- cbalTree (half + 1)
      r <- cbalTree half
      return $ [Branch 'x' l r, Branch 'x' r l]
      where
        half = (n - 1) `div` 2

{-
Problem 56
(**) Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

Example in Haskell:

*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
*Main> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
True
-}
symmetric :: Eq a => Tree a -> Bool
symmetric Empty                  = True
symmetric (Branch _ Empty Empty) = True
symmetric (Branch _ _ Empty)     = False
symmetric (Branch _ Empty _)     = False
symmetric (Branch _ l r)         = l `isMirrorOf` r
  where
    isMirrorOf Empty Empty = True
    isMirrorOf (Branch _ l r) (Branch _ l' r') = l `isMirrorOf` r' && r `isMirrorOf` l'
    isMirrorOf _ _ = False

{-
Problem 57
(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.

Example:

* construct([3,2,5,7,1],T).
T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
Then use this predicate to test the solution of the problem P56.

Example:

* test-symmetric([5,3,18,1,4,12,21]).
Yes
* test-symmetric([3,2,5,7,4]).
No
Example in Haskell:

*Main> construct [3, 2, 5, 7, 1]
Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
*Main> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
True
*Main> symmetric . construct $ [3, 2, 5, 7, 1]
True
-}
add :: Int -> Tree Int -> Tree Int
add n Empty = leaf n
add n (Branch n' l r)
  | n < n' = Branch n' (add n l) r
  | n > n' = Branch n' l (add n r)
  | otherwise = error "n already presented in the tree!"

construct :: [Int] -> Tree Int
construct ns = foldl' (\acc n -> add n acc) Empty ns

