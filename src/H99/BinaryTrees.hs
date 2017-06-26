module H99.BinaryTrees where

import           Control.Monad (replicateM)

data Tree a = Empty
            | Branch a (Tree a) (Tree a)
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
      [l, r] <- replicateM 2 $ cbalTree $ (n - 1) `div` 2
      return $ Branch 'x' l r
    else concat $ do
      l <- cbalTree (half + 1)
      r <- cbalTree half
      return $ [Branch 'x' l r, Branch 'x' r l]
      where
        half = (n - 1) `div` 2
