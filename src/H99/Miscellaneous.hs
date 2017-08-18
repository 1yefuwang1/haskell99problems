module H99.Miscellaneous where

import           Control.Monad (guard)

{-
Eight queens problem

This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

Example in Haskell:

> length (queens 8)
92
> head (queens 8)
[1,5,8,6,3,7,2,4]
-}
queens :: Int -> [[Int]]
queens n = placeQueens n
  where
    placeQueens 0 = return []
    placeQueens n' = do
      queen <- [1..n]
      queens' <- placeQueens (n'-1)
      guard $ isSafe queen queens'
      return $ queen : queens'

    isSafe :: Int -> [Int] -> Bool
    isSafe row qs = row `notElem` qs && all (notDiagonalWith row) (zip qs [1..(length qs)])
      where
        notDiagonalWith row (row', col) = col /= abs (row' - row)

