module H99.BinaryTrees where

import           Control.Monad (replicateM)
import           Data.List     (foldl')

data Tree a = Empty
            | Branch {value :: a, left :: Tree a, right :: Tree a}
            deriving (Show, Eq, Ord)

leaf :: a -> Tree a
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
symmetric :: Tree a -> Bool
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

{-
Problem 58
(**) Generate-and-test paradigm

Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

Example:

* sym-cbal-trees(5,Ts).
Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]
Example in Haskell:

*Main> symCbalTrees 5
[Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
-}
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ cbalTree n

{-
Problem 59
(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and the given maximum height.

Example:

?- hbal_tree(3,T).
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
etc......No
Example in Haskell:

*Main> take 4 $ hbalTree 'x' 3
[Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
-}
-- default to Tree Char for simplicity
hbalTree :: Int -> [Tree Char]

hbalTree 0 = return $ Empty
hbalTree n
  | n < 0 = []
  | otherwise =
    [Branch 'x' l r | (hl, hr) <- [(n-1, n-1), (n-1, n-2), (n-2, n-1)] , l <- hbalTree hl, r <- hbalTree hr]


{-
Problem 60
(**) Construct height-balanced binary trees with a given number of nodes

Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?

Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H. On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this.
Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. Find out how many height-balanced trees exist for N = 15.

Example in Prolog:

?- count_hbal_trees(15,C).
C = 1553
Example in Haskell:

*Main> length $ hbalTreeNodes 'x' 15
1553
*Main> map (hbalTreeNodes 'x') [0..3]
[[Empty],
 [Branch 'x' Empty Empty],
 [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
 [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
-}
type Height = Int
minNodes :: Height -> Int
minNodes 0 = 0
minNodes 1 = 1
minNodes n = 1 + minNodes (n-1) + minNodes (n-2)

type NodeNumber = Int
maxHeight :: NodeNumber -> Height
maxHeight 0 = 0
maxHeight 1 = 1
maxHeight n =
  let h = maxHeight (n-1) in
  if maxHeight (n - minNodes (h-1) - 1) == h
    then h + 1
    else h

minHeight :: NodeNumber -> Height
minHeight n = ceiling $ logBase 2 $ fromIntegral $ n + 1

-- default to Tree Char for simplicity
hbalTreeNodes :: Int -> [Tree Char]
hbalTreeNodes nodeNum =
  filter ((nodeNum ==) . countNodes) $ concatMap (hbalTree) [minHeight nodeNum..maxHeight nodeNum]
  where
    countNodes Empty          = 0
    countNodes (Branch _ l r) = 1 + countNodes l + countNodes r


{-
Problem 61
Count the leaves of a binary tree

A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.

Example:

% count_leaves(T,N) :- the binary tree T has N leaves
Example in Haskell:

> countLeaves tree4
2
-}
countLeaves :: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r

{-
Problem 61A
Collect the leaves of a binary tree in a list

A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

Example:

% leaves(T,S) :- S is the list of all leaves of the binary tree T
Example in Haskell:

> leaves tree4
[4,2]
-}
leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ l r)         = leaves l ++ leaves r

{-
 Problem 62
Collect the internal nodes of a binary tree in a list

An internal node of a binary tree has either one or two non-empty successors. Write a predicate internals/2 to collect them in a list.

Example:

% internals(T,S) :- S is the list of internal nodes of the binary tree T.
Example in Haskell:

Prelude>internals tree4
Prelude>[1,2]
-}
internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch x Empty Empty) = []
internals (Branch x l r)         = x : (internals l ++ internals r)

{-
Problem 62B
Collect the nodes at a given level in a list

A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.

Example:

% atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
Example in Haskell:

Prelude>atLevel tree4 2
Prelude>[2,2]
-}
atLevel :: Tree a -> Int -> [a]
atLevel tree level = doAtLevel tree level 1
  where
    doAtLevel Empty _ _ = []
    doAtLevel (Branch x l r) targetLevel curLevel
      | targetLevel == curLevel = return x
      | targetLevel > curLevel = doAtLevel l targetLevel nextLevel ++ doAtLevel r targetLevel nextLevel
      | otherwise = error "targetLevel < curLevel considered impossible"
      where
        nextLevel = curLevel + 1
