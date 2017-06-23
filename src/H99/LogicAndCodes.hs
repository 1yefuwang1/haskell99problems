module H99.LogicAndCodes where

import           Control.Monad (replicateM)

{-
Problem 46
(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

Example:

(table A B (and A (or A B)))
true true true
true fail true
fail true fail
fail fail fail
Example in Haskell:

> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False
-}

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _       = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _    = False

nand' :: Bool -> Bool -> Bool
nand' a = not . and' a

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _         = False

xor' :: Bool -> Bool -> Bool
xor' True True   = False
xor' False False = True

-- what is impl ?

equ' :: Bool -> Bool -> Bool
equ' True True   = True
equ' False False = True
equ' _ _         = False

table :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table f = do
  a <- t
  b <- t
  return (a, b, f a b)
  where
    t = [True, False]

{-
Problem 47
(*) Truth tables for logical expressions (2).

Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.

Example:

* (table A B (A and (A or not B)))
true true true
true fail true
fail true fail
fail fail fail
Example in Haskell:

> table2 (\a b -> a `and'` (a `or'` not b))
True True True
True False True
False True False
False False False
-}
-- Haskell functions are infix operators, no work to do
table' :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table' = table

{-
Problem 48
(**) Truth tables for logical expressions (3).

Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.

Example:

* (table (A,B,C) (A and (B or C) equ A and B or A and C))
true true true true
true true fail true
true fail true true
true fail fail true
fail true true true
fail true fail true
fail fail true true
fail fail fail true
Example in Haskell:

> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- infixl 3 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False True
False True  True  True
False True  False True
False False True  True
False False False True

-- infixl 7 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False False
False True  True  False
False True  False False
False False True  False
False False False False
-}
tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = do
  c <- comb
  return $ c ++ [f c]
  where
    comb = replicateM n [True, False]

{-
Problem 49
(**) Gray codes.

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
Find out the construction rules and write a predicate with the following specification:

% gray(N,C) :- C is the N-bit Gray code
Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?

Example in Haskell:

P49> gray 3
["000","001","011","010","110","111","101","100"]
-}

gray :: Int -> [[Char]]
gray 1 = ["0", "1"]
gray n =
  if n <= 0
    then error "n should be greater than 0"
    else doGray
  where
    doGray = prepend' ('0') prev ++ prepend' ('1') (reverse prev)
    prepend' x = map $ (x:)
    prev = gray (n-1)
