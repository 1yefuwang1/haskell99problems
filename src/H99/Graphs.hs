{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module H99.Graphs where

import           Control.Exception   (assert)
import           Control.Monad.State
import           Data.List           (foldl', minimumBy, nub, partition,
                                      permutations, sort, sortBy)
import           Data.Map            ((!))
import qualified Data.Map            as M
import           Data.Ord            (comparing)
import qualified H99.MultiwayTrees   as T
import           System.IO.Unsafe    (unsafePerformIO)

type Node a = a
type Edge a = (a, a)
type Nodes a = [Node a]
type Edges a = [Edge a]

data Graph a
  = Graph (Nodes a) (Edges a)
  | Adj [(Node a, Nodes a)]
  deriving (Show)

normalize :: (Eq a, Ord a) => Edges a -> Edges a
normalize =
  foldr
    (\(a, b) acc ->
      if a > b then (b, a) : acc else (a, b) : acc
    )
    []
normalize' :: (Eq a, Ord a) => Edges a -> Edges a
normalize' = sort . normalize


instance (Eq a, Ord a) => Eq (Graph a) where
  (==) ::  Graph a -> Graph a -> Bool
  Graph ns es == Graph ns' es' = (sort ns == sort ns') && es `equals` es'
    where
      equals :: (Eq a, Ord a) => Edges a -> Edges a -> Bool
      left `equals` right = normalize' left == normalize' right

  Adj es == Adj es' = es == es'
  _ == _ = False

type LabelledEdge a b = (a, a, b)
data LabelledGraph a b
  = LabelledGraph
    { nodes :: Nodes a
    , edges :: [LabelledEdge a b]
    }
  deriving (Show)

instance (Eq a, Ord a, Eq b, Ord b) => Eq (LabelledGraph a b) where
  (==) :: LabelledGraph a b -> LabelledGraph a b -> Bool
  LabelledGraph ns es == LabelledGraph ns' es' = (sort ns == sort ns') && es `equals` es'
    where
      equals :: (Eq a, Ord a, Eq b, Ord b) => [LabelledEdge a b] -> [LabelledEdge a b] -> Bool
      left `equals` right = normalize' left == normalize' right
        where
          normalize' :: (Eq a, Ord a, Eq b, Ord b) => [LabelledEdge a b] -> [LabelledEdge a b]
          normalize' = sort . normalize

          normalize :: (Eq a, Ord a, Eq b, Ord b) => [LabelledEdge a b] -> [LabelledEdge a b]
          normalize =
            foldr
              (\(from, to, value) acc ->
                if from > to then (to, from, value) : acc else (from, to, value) : acc
              )
              []

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
Problem 83
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
-- To simplify the problem, I use a Graph, which satisfy invariant: |V| -1 == |E|, to represent a spanning tree.
spantree :: (Eq a, Ord a) => Graph a -> [Graph a]
spantree g@(Graph ns es) = nub $ go [] ns [] g
  where
    go :: Eq a
       => Nodes a  -- visited nodes
       -> Nodes a  -- unvisited nodes
       -> Edges a  -- collected edges so far
       -> Graph a  -- graph
       -> [Graph a]
    go visited [] edges (Graph ns es) = assert (length visited == length ns && length ns -1 == length es) $
      return $ Graph visited edges

    go [] unvisited [] g@(Graph ns es) = assert (unvisited == ns) $ do
      -- n <- unvisited
      let n = head unvisited
      go [n] [n' | n' <- unvisited, n' /= n] [] g

    go visited unvisited edges g@(Graph ns es) = do
      let
        adjEdges = [(n', n'') | n' <- unvisited, n'' <- visited, (n', n'') `elem` es || (n'', n') `elem` es]
      adjEdge <- adjEdges
      let
        (fst', snd') = adjEdge
        (visited', unvisited') =
          if fst' `elem` visited
            then (fst', snd')
            else assert (snd' `elem` visited) (snd', fst')
      go (unvisited':visited) [n' | n' <- unvisited, unvisited' /= n'] (adjEdge:edges) g

isTree :: (Eq a, Ord a) => Graph a -> Bool
isTree = (== 1) . length . spantree

isConnected :: (Eq a, Ord a) => Graph a -> Bool
isConnected = not . null . spantree

{-
Problem 84
(**) Construct the minimal spanning tree

Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning tree of a given labelled graph. Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick. The data of the example graph to the right can be found in the file p84.dat.

Example in Haskell:

prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
[(1,2,12),(1,3,34),(2,4,55),(2,5,32)]
-}
-- still use a LabelledGraph to represent the minimal spanning tree
prim :: (Eq a, Ord a, Eq b, Ord b) => LabelledGraph a b -> LabelledGraph a b
prim g@(LabelledGraph ns es) = go [] ns [] g
  where
    go :: (Eq a, Ord a, Eq b, Ord b)
       => Nodes a  -- visited nodes
       -> Nodes a  -- unvisited nodes
       -> [LabelledEdge a b]  -- collected edges so far
       -> LabelledGraph a b -- graph
       -> LabelledGraph a b
    go visited [] edges (LabelledGraph ns es) = assert (length visited == length ns && length ns -1 == length es) $
      LabelledGraph visited edges

    go [] unvisited [] g@(LabelledGraph ns es) = assert (unvisited == ns) $
      let
        n = head unvisited
      in
      go [n] [n' | n' <- unvisited, n' /= n] [] g

    go visited unvisited edges g@(LabelledGraph ns es) =
      let
        adjEdges =
          [(n', n'', v)
          | n' <- unvisited
          , n'' <- visited
          , (n1, n2, v) <- es
          , (n', n'') == (n1, n2) || (n'', n') == (n1, n2)
          ]
        minAdjEdge = minimumBy (\(_, _, v) (_, _, v') -> compare v v') adjEdges
        (fst', snd', v) = minAdjEdge
        (visited', unvisited', value) =
          if fst' `elem` visited
            then (fst', snd', v)
            else assert (snd' `elem` visited) (snd', fst', v)
      in
      go (unvisited':visited) [n' | n' <- unvisited, unvisited' /= n'] (minAdjEdge:edges) g

{-
Problem 85
Graph isomorphism

Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent if and only if f(X) and f(Y) are adjacent.

Write a predicate that determines whether two graphs are isomorphic. Hint: Use an open-ended list to represent the function f.

Example in Haskell:

graphG1 = [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
iso graphG1 graphH1
True
-}
-- Because solution in Polynomial-time  to determining graph isomorphism is still subject to ongoing research,
-- I exhaust every possible mapping below
type Mapping a b = M.Map (Node a) (Node b)
iso :: forall a b. (Eq a, Ord a, Eq b, Ord b) => Graph a -> Graph b -> Bool
iso left@(Graph ns es) right@(Graph ns' es')
  | (length ns, length es) /= (length ns', length es') = False
  | otherwise = any isTrue $ map ((== left) . transform) mappings
  where
    isTrue = id

    -- transform the right-hand-side Graph according to the given mapping
    transform :: Mapping b a -> Graph a
    transform mapping = Graph ns'' es''
      where
        ns'' = [mapping ! n | n <- ns']
        es'' = [mapTuple (mapping !) (n1, n2) | (n1, n2) <- es']

        mapTuple :: (b -> a) -> (b, b) -> (a, a)
        mapTuple f (l, r) = (f l, f r)

    -- mapping from ns' to ns
    mappings :: [Mapping b a]
    mappings =  [M.fromList (zip ns' perm) | perm <- permutations ns]

{-
Problem 86
(**) Node degree and graph coloration

a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a given node.

b) Write a predicate that generates a list of all nodes of a graph sorted according to decreasing degree.

c) Use Welch-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have different colors.

Example in Haskell:

kcolor ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
[('a',1),('b',2),('c',1),('d',2),('e',3),('f',2),('g',1),('h',3),('i',3),('j',2)]
-}
type Color = Int
type Degree = Int

degree :: Eq a =>Graph a -> Node a -> Degree
degree g@(Graph ns es) n = assert (n `elem` ns) $
  foldr (\(n1, n2) acc -> if n1 == n || n2 == n then 1 + acc else acc) 0 es

sortByDegreeDesc :: Eq a => Graph a -> [(a, Degree)]
sortByDegreeDesc g@(Graph ns es) = sortBy (comparing (negate . snd)) [(n, degree g n) | n <- ns]

kcolor :: forall a. (Eq a, Show a) => Graph a -> [(a, Color)]
kcolor g@(Graph ns es) = go 1 (sortByDegreeDesc g) []
  where
    go :: Color -> [(a, Degree)] -> [(a, Color)] -> [(a, Color)]
    go _ [] acc = acc
    go color ds acc =
      let
        (currentNode, _) = head ds
        isConnectedTo :: Node a -> Node a -> Bool
        n1 `isConnectedTo` n2 = (n1, n2) `elem` es || (n2, n1) `elem` es
        isConnectedToAll :: Node a -> Nodes a -> Bool
        n1 `isConnectedToAll` ns = any (isConnectedTo n1) ns
        (colorred, rest) =
          foldl'
            (\acc@(colorred', rest') element@(n, d) ->
                if n `isConnectedToAll` fmap fst colorred'
                  then (colorred', element : rest')
                  else ((n, color) : colorred', rest')
            )
            ([(currentNode, color)], [])
            (tail ds)
      in
        go (color + 1) (reverse rest) (colorred ++ acc)
