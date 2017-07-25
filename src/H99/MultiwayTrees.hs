module H99.MultiwayTrees where

import           Control.Monad.State

data Tree a = Node {value :: a, children :: [Tree a]}
            deriving (Eq, Show)

{-
 Problem 70C
(*) Count the nodes of a multiway tree.

Example in Haskell:

Tree> nnodes tree2
2
-}
nnodes :: Tree a -> Int
nnodes (Node _ children) = 1 + (sum . fmap nnodes) children

{-
Problem 70
(**) Tree construction from a node string.

We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.

By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^

p70.gif

Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.

Example in Haskell:

Tree> stringToTree "afg^^c^bd^e^^^"
Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]

Tree> treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
"afg^^c^bd^e^^^"
-}
treeToString :: Tree Char -> String
treeToString (Node v children) = v : childStr ++ "^"
  where
    childStr = concatMap treeToString children

stringToTree :: String -> Tree Char
stringToTree (h:rest) = Node h (evalState consume ([], take (length rest - 1) rest))
  where
    consume :: State ([Tree Char], String) [Tree Char]
    consume = do
      (trees, remaining) <- get
      if null remaining
        then return trees
        else do
          let (node, remaining') = consumeOneNode remaining [] -- use [] to represent Empty Node
          put (trees ++ node, remaining')
          consume

    consumeOneNode :: String
                   -> [Tree Char] -- is either Empty or consisting of only one element
                   -> ([Tree Char], String)
    consumeOneNode (h:rest) tree =
      case h of
        '^' -> (tree, rest)
        _ ->
          if null tree
            then consumeOneNode rest [Node h []]
            else
              let
                t = head tree
                v = value t
                c = children t
                (t', rest') = consumeOneNode rest []
              in
                consumeOneNode rest' [Node v (c ++ [Node h ([] ++ t')])]


{-
Problem 71
(*) Determine the internal path length of a tree.

We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.

Example in Haskell:

Tree> ipl tree5
9
Tree> ipl tree4
2
-}
type Depth = Int
ipl :: Tree a -> Int
ipl t = evalState go (0, t)
  where
    go :: State (Depth, Tree a) Int
    go = do
      (depth, t) <- get
      case t of
        Node _ [] -> return depth
        Node _ cs -> return $ (+depth) . sum $ map (evalState go) [(depth + 1, c) | c <- cs]

{-
Problem 72
(*) Construct the bottom-up order sequence of the tree nodes.

Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the multiway tree Tree.

Example in Haskell:

Tree> bottom_up tree5
-}
bottomUp :: Tree Char -> String
bottomUp (Node ch []) = [ch]
bottomUp (Node ch cs) = concatMap bottomUp cs ++ [ch]
