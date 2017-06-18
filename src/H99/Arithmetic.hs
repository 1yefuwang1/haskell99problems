module H99.Arithmetic where

{-
Problem 31
(**) Determine whether a given integer number is prime.

Example:

* (is-prime 7)
T
Example in Haskell:

P31> isPrime 7
True
-}
isPrime :: Int -> Bool
isPrime n = n `elem` (take n primes)
  where
    primes = filterPrime [2..]
    filterPrime (x:xs) = x : filterPrime [x' | x' <- xs, x' `mod` x /= 0]
