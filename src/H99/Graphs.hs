module H99.Graphs where

type Node a = a
type Edge a = (a, a)
type Nodes a = [Node a]
type Edges a = [Edge a]

data Graph a
  = Graph (Nodes a, Edges a)
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
graphToAdj (Graph ([], [])) = Adj []
graphToAdj (Graph (nodes, edges)) = Adj $ do
  n <- nodes
  let es = map f $ filter (\(n1, n2) -> n1 == n || n2 ==n) edges
      f (n1, n2)
        | n1 == n = n2
        | n2 == n = n1
  return (n, es)
