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
bottomUp (Node ch cs) = concatMap bottomUp cs ++ [ch]

{-
Problem 73
(**) Lisp-like tree representation.

There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language, which is used primarily for artificial intelligence problems. As such it is one of the main competitors of Prolog. In Lisp almost everything is a list, just as in Prolog everything is a term.

The following pictures show how multiway tree structures are represented in Lisp.

p73.png

Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')', which we shall collectively call "tokens". We can represent this sequence of tokens as a Prolog list; e.g. the lispy expression (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list" LTL if the tree is given as term T in the usual Prolog notation.

(The Prolog example given is incorrect.)

Example in Haskell:

Tree> display lisp tree1
"a"
Tree> display lisp tree2
"(a b)"
Tree> display lisp tree3
"(a (b c))"
Tree> display lisp tree4
"(b d e)"
Tree> display lisp tree5
"(a (f g) c (b d e))"
As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse conversion is also possible.
-}
lisp :: Tree Char -> String
lisp (Node v []) = [v]
lisp (Node v cs) = '(' : ([v, ' '] ++ unwords (map lisp cs) ++ ")")
