module H99.BinaryTrees where

import           Control.Monad       (replicateM)
import           Control.Monad.State
import           Data.List           (foldl', sort)

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
cbalTree 0 = return Empty
cbalTree 1 = return $ leaf 'x'
cbalTree n =
  if odd n
    then do
      [l, r] <- replicateM 2 $ cbalTree half
      return $ Branch 'x' l r
    else concat $ do
      l <- cbalTree (half + 1)
      r <- cbalTree half
      return [Branch 'x' l r, Branch 'x' r l]
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
construct = foldl' (flip add) Empty

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

hbalTree 0 = return Empty
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
  filter ((nodeNum ==) . countNodes) $ concatMap hbalTree [minHeight nodeNum..maxHeight nodeNum]
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

{-
Problem 63
Construct a complete binary tree

A complete binary tree with height H is defined as follows:

The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.

Write a predicate complete_binary_tree/2.

Example:

% complete_binary_tree(N,T) :- T is a complete binary tree with N nodes.
Example in Haskell:

Main> completeBinaryTree 4
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)

Main> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
True
-}
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = construct n 1
  where
    construct target cur
      | target < cur = Empty
      | otherwise = Branch 'x' (construct target leftIndex) (construct target rightIndex)
      where
        leftIndex = 2 * cur
        rightIndex = leftIndex + 1

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree tree  = (isAscending . sort) (traverseAndCollect tree 1) 1
isAscending :: [Int] -> Int -> Bool
isAscending [] _     = True
isAscending (x:xs) n = x == n && isAscending xs (n+1)

traverseAndCollect :: Tree a -> Int -> [Int]
traverseAndCollect Empty _ = []
traverseAndCollect (Branch _ l r) index = [index] ++ (traverseAndCollect l leftIndex ++ traverseAndCollect r rightIndex)
  where
    leftIndex = index * 2
    rightIndex = leftIndex + 1

{-
Problem 64
Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid. Several layout methods are conceivable, one of them is shown in the illustration below:

p64.gif

In this layout strategy, the position of a node v is obtained by the following two rules:

x(v) is equal to the position of the node v in the inorder sequence
y(v) is equal to the depth of the node v in the tree
Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or the rectangle bounding the drawn tree.

Here is the example tree from the above illustration:

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )
Example in Haskell:

> layout tree64
Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...
-}
type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = fst $ evalState (go t) (1, 1)
  where
    go :: Tree a -> State Pos (Tree (a, Pos), Int)
    go Empty = do
      (x, _) <- get
      return (Empty, x)

    go (Branch v l r) = do
      (x, y) <- get
      put (x, y+1)
      (l', x') <- go l
      put (x'+1, y+1)
      (r', x'') <- go r
      return (Branch (v, (x', y)) l' r', x'')

{-
layout :: Tree a -> Tree (a, Pos)
layout t = fst (layoutAux 1 1 t)
  where layoutAux x y Empty = (Empty, x)
        layoutAux x y (Branch a l r) = (Branch (a, (x',y)) l' r', x'')
          where (l', x')  = layoutAux x (y+1) l
                (r', x'') = layoutAux (x'+1) (y+1) r
-}

{-
Problem 65
An alternative layout method is depicted in the illustration below:

p65.gif

Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance between neighboring nodes is constant.

Use the same conventions as in problem P64 and test your function in an appropriate way.

Here is the example tree from the above illustration:

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )
Example in Haskell:

> layout tree65
Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...
-}
layout' :: Tree a -> Tree (a, Pos)
layout' t = fst $ evalState (go t) (1, 1)
  where
    depth = getMaxDepth t

    getMaxDepth Empty          = 0
    getMaxDepth (Branch _ l r) = 1 + max (getMaxDepth l) (getMaxDepth r)

    go :: Tree a -> State Pos (Tree (a, Pos), Int)
    go Empty = do
      (x, _) <- get
      return (Empty, x)

    go (Branch v l r) = do
      (x, y) <- get
      put (x, y + 1)
      (l', x') <- go l
      let deltaUp = 2^(depth - y)
      let deltaDown = deltaUp `div` 2
      let r' = construct' (x' + deltaDown, y + 1) r
      return (Branch (v, (x', y)) l' r', x' + deltaUp)

    construct' :: (Int, Int) -> Tree a -> Tree (a, Pos)
    construct' _ Empty = Empty
    construct' (x, y) (Branch v l r) =
      Branch (v, (x, y)) (construct' (x - delta, y + 1) l) (construct' (x + delta, y + 1) r)
      where
        delta = 2^(depth - y - 1)

{-
Problem 66
Yet another layout strategy is shown in the illustration below:

p66.gif

The method yields a very compact layout while maintaining a certain symmetry in every node. Find out the rules and write the corresponding Prolog predicate. Hint: Consider the horizontal distance between a node and its successor nodes. How tight can you pack together two subtrees to construct the combined binary tree?

Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way. Note: This is a difficult problem. Don't give up too early!

Which layout do you like most?

Example in Haskell:

> layout tree65
Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...
-}
layout'' = undefined -- need time to figure it out.

{-
Problem 67A
A string representation of binary trees

Somebody represents binary trees as strings of the following type:

a(b(d,e),c(,f(g,)))
a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.

Example in Prolog

?- tree_to_string(t(x,t(y,nil,nil),t(a,nil,t(b,nil,nil))),S).
S = 'x(y,a(,b))'
?- string_to_tree('x(y,a(,b))',T).
T = t(x, t(y, nil, nil), t(a, nil, t(b, nil, nil)))
Example in Haskell:

Main> stringToTree "x(y,a(,b))" >>= print
Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
Main> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
True
-}
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c l r) = c : ("(" ++ treeToString l ++ "," ++ treeToString r ++ ")")

type Index = Int --Index of the correct ','
type LeftParathesisEncountered = Int
stringToTree :: String -> Maybe (Tree Char)
stringToTree ""  = Just Empty
stringToTree [c] = Just $ Branch c Empty Empty
stringToTree (c:cs) = do
    cs' <- trimmedStr
    (leftStr, rightStr) <- evalStateT (splitAtCorrectComma cs') (0, 0)
    l <- stringToTree leftStr
    r <- stringToTree (tail rightStr) --remove the leading ','
    return $ Branch c l r
  where
    trimmedStr = removeEnclosingParathesis cs

    removeEnclosingParathesis :: String -> Maybe String
    removeEnclosingParathesis [] = Nothing
    removeEnclosingParathesis [h] = Nothing
    removeEnclosingParathesis (h:rest) = case (h, last rest) of
      ('(', ')') -> Just $ take (length rest - 1) rest
      _          -> Nothing

    splitAtCorrectComma :: String -> StateT (Index, LeftParathesisEncountered) Maybe (String, String)
    splitAtCorrectComma [] = lift Nothing
    splitAtCorrectComma (c:rest) = do
      (i, e) <- get
      case (c, e) of
        (',', 0) -> lift . fmap (splitAt i) $ trimmedStr
        ('(', _) -> do
          put (i + 1, e + 1)
          splitAtCorrectComma rest
        (')', n) -> do
          put (i + 1, n - 1)
          splitAtCorrectComma rest
        (_, n) -> do
          put (i + 1, n)
          splitAtCorrectComma rest
        _ -> lift Nothing



{-
Problem 68
Preorder and inorder sequences of binary trees. We consider binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67.

a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the example in problem P67.

b) Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder sequence, construct a corresponding tree? If not, make the necessary arrangements.

c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given, then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.

Example in Haskell:

Main> let { Just t = stringToTree "a(b(d,e),c(,f(g,)))" ;
            po = treeToPreorder t ;
            io = treeToInorder t } in preInTree po io >>= print
Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
-}
-- a)
preorder :: Tree Char -> String
preorder Empty          = ""
preorder (Branch c l r) = c : (preorder l ++ preorder r)

inorder :: Tree Char -> String
inorder Empty          = ""
inorder (Branch c l r) = inorder l ++ [c] ++ inorder r

-- b)
preorderToTree :: String -> Tree Char
preorderToTree "" = Empty
preorderToTree (h:rest) = Branch h (preorderToTree leftHalf) (preorderToTree rightHalf)
  where
    (leftHalf, rightHalf) = splitAt (length rest `div` 2) rest

-- c)
type PreOrderSeq = String
type InOrderSeq = String
preInTree :: PreOrderSeq -> InOrderSeq -> Tree Char
preInTree [] [] = Empty

preInTree (h:preorder) inorder =
  let
    (l, r') = span (/=h) inorder
    r = tail r'
    (l'', r'') = splitAt (length l) preorder
  in
    Branch h (preInTree l'' l) (preInTree r'' r)

preInTree _ _ = error "invalid input sequence"


{-
 Problem 69
Dotstring representation of binary trees.

We consider again binary trees with nodes that are identified by single lower-case letters, as in the example of problem P67. Such a tree can be represented by the preorder sequence of its nodes in which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal. For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'. First, try to establish a syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which does the conversion in both directions. Use difference lists.

Example in Haskell:

> fst (ds2tree example)
Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

> tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))
"xy..z0..."
-}
-- TODO: use difference list
tree2ds :: Tree Char -> String
tree2ds Empty          = "."
tree2ds (Branch c l r) = c : (tree2ds l ++ tree2ds r)

ds2tree :: String -> Tree Char
ds2tree "." = Empty
ds2tree (h:rest) = Branch h l r
  where
    (l, remaining) = evalState consumeOneTree rest
    r = ds2tree remaining

    consumeOneTree :: State String (Tree Char, String)
    consumeOneTree = do
      (h:rest) <- get
      case h of
        '.' -> return (Empty, rest)
        _ -> do
          put rest
          (l', rest') <- consumeOneTree
          put rest'
          (r', rest'') <- consumeOneTree
          return (Branch h l' r', rest'')


