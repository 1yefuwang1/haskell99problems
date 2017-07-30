module H99.Graphs where

import           Control.Monad.State

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
paths from to g = evalState (go from to g) [from]
  where
    go :: Int
       -> Int
       -> Graph Int
       -> State [Int] [[Int]] -- State [Int] records the nodes that
                              -- has been traversed to eliminate acyclic go
    go from to (Graph ns es) = do
      visited <- get
      if from == to
        then
          return $ return [to]
        else do
          let
            adjNodes =
              foldr
                (\(n1, n2) acc ->
                  if n1 == from && n2 `notElem` visited
                    then n2 : acc
                    else if n2 == from && n1 `notElem` visited
                      then n1 : acc
                      else acc)
                []
                es
          return $ do
            adjNode <- adjNodes
            path <- evalState (go adjNode to (Graph ns es)) (adjNode:visited)
            return $ from : path

