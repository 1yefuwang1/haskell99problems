module H99.Arithmetic where

import           Data.List

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

{-
Problem 33
(*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

Example:

* (coprime 35 64)
T
Example in Haskell:

* coprime 35 64
True
-}
coprime a b = myGCD a b == 1

{-
Problem 34
(**) Calculate Euler's totient function phi(m).

Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

Example:

* (totient-phi 10)
4
Example in Haskell:

* totient 10
4
-}
totient :: Int -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..n-1]


 {-
 Problem 35
(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

Example:

* (prime-factors 315)
(3 3 5 7)
Example in Haskell:

> primeFactors 315
[3, 3, 5, 7]
 -}
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = spf : primeFactors (n `div` spf)
  where
    spf = smallestPrimeFactor primes n
    smallestPrimeFactor (p:ps) n =
      if n `mod` p == 0
        then p
        else smallestPrimeFactor ps n
    primes = filterPrime [2..]
    filterPrime (x:xs) = x : filterPrime [x' | x' <- xs, x' `mod` x /= 0]
primeFactors _ = error "invalid input"

{-
Problem 36
(**) Determine the prime factors of a given positive integer.

Construct a list containing the prime factors and their multiplicity.

Example:

* (prime-factors-mult 315)
((3 2) (5 1) (7 1))
Example in Haskell:

*Main> prime_factors_mult 315
[(3,2),(5,1),(7,1)]
-}
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = toPair . group . primeFactors
  where toPair = map (\x -> (head x, length x))
