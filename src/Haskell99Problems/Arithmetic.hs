module Haskell99Problems.Arithmetic where

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

{-
Problem 32
(**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

Example:

* (gcd 36 63)
9
Example in Haskell:

[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]
-}
myGCD :: Integral t => t -> t -> t
myGCD a b
  | b == 0 = abs a
  | r == 0 = abs b
  | otherwise = myGCD b r
  where r = a `mod` b
