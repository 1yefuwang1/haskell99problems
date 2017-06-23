module H99.LogicAndCodes where

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
  a <- table'
  b <- table'
  return (a, b, f a b)
  where
    table' = [True, False]

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
-- Haskell functions are automatically infix operators
table' :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table' = table
