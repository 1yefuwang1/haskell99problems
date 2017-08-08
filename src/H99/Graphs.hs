{-# LANGUAGE MultiWayIf #-}
module H99.Graphs where

import           Control.Exception   (assert)
import           Control.Monad.State
import qualified H99.MultiwayTrees   as T

type Node a = a
type Edge a = (a, a)
type Nodes a = [Node a]
type Edges a = [Edge a]

data Graph a
  = Graph (Nodes a) (Edges a)
  | Adj [(Node a, Nodes a)]
  deriving (Show, Eq)

{-
Problem 80
(***) Conversions

Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.

Example in Haskell:

graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
-}
graphToAdj :: Eq a => Graph a -> Graph a
graphToAdj (Graph [] []) = Adj []
graphToAdj (Graph nodes edges) = Adj $ do
  n <- nodes
  let es = map f $ filter (\(n1, n2) -> n1 == n || n2 ==n) edges
      f (n1, n2)
        | n1 == n = n2
        | n2 == n = n1
  return (n, es)

{-
Problem 81
(**) Path from one node to another one

Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.

Example in Haskell:

paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[1,2,3,4],[1,3,4]]
paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
-}

paths :: Int -> Int -> Graph Int -> [[Int]]
paths from to g = evalState (go from to g) []
  where
    go :: Int
       -> Int
       -> Graph Int
       -> State [Int] [[Int]] -- State [Int] records the nodes that
                              -- has been traversed to eliminate acyclic go
    go from to (Graph ns es) = do
      modify (from:)
      visited <- get
      if from == to
        then
          return $ return [to]
        else do
          let
            adjNodes =
              foldr
                (\(n1, n2) acc ->
                  if | n1 == from && n2 `notElem` visited -> n2 : acc
                     | n2 == from && n1 `notElem` visited -> n1 : acc
                     | otherwise -> acc)
                []
                es
          return $ do
            adjNode <- adjNodes
            path <- evalState (go adjNode to (Graph ns es)) visited
            return $ from : path

{-
Problem 82
(*) Cycle from a given node

Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. The predicate should return all cycles via backtracking.

Example in Haskell:

graph> cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[2,3,4,2]]
graph> cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
-}
cycle' :: Int -> Graph Int -> [[Int]]
cycle' from g@(Graph ns es) = do
    let
      adjNodes = filter (\n -> (from, n) `elem` es || (n, from) `elem` es) ns
    n <- adjNodes
    path <- filter ((/=n) . secondLast) $ paths n from g
    return $ from:path
  where
    secondLast [x, y] = x
    secondLast (x:xs) = secondLast xs

{-
Construct all spanning trees

Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph.
With this predicate, find out how many spanning trees there are for the graph depicted to the left.
The data of this example graph can be found in the file p83.dat.
When you have a correct solution for the s_tree/2 predicate,
use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph).
Both are five-minutes tasks!
length $ spantree k4
16
-}
spantree :: Eq a => Graph a -> [T.Tree a]
spantree (Graph [n] []) = [T.Node n []]
spantree (Graph [] _)   = []
spantree (Graph _ [])   = []
spantree (Graph ns es) =  do
  n <- ns
  let
    ns' = [n' | n' <- ns, n' /= n]
    adjNodes = [n' | n' <- ns', (n, n') `elem` es || (n', n) `elem` es]
  adjNode <- adjNodes
  tree <- go [n]  adjNodes es
  return $ T.Node n tree

 where
  go :: Nodes a -> Nodes a -> Edges a -> [T.Tree a]
  go visitedNodes adjNodes edges = undefined
