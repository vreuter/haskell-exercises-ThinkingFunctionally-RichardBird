{-
6D: Cartesian product as a fold -- implement Cartesian product as foldr f e, 
selecting suitable values for the function f to fold/apply and initial accumulator e.

Vince Reuter
August 2019
-}

module CartesianProduct (cp, cp') where

-- Cartesian product over a collection of input lists
{-
BRAINSTORMING / Thought process:
To express any arbitrary operation as a fold, we need to choose the proper pair of 
function to "fold in" (f) and identity element / initial accumulator (e). 
In the case of a Cartsian product, clearly the result type is a list (of lists), 
so we need to:

1. Begin with an empty list (i.e., [] will be our initial accumulator (e).)

Observation: each list in a cartesian product is a "path" through the input lists, 
comprised of exactly 1 element from each input list (like a vector space defined as a direct sum of subspaces)

2. Since we should PRESERVE ORDER (in accordance with the typical tuple representation of elements in the 
Cartesian product), each list in the product can be created by using the (:) operation between elements 
in the "path" taken through the collection of input lists.

Cases to consider:
a. empty input (base case)
b. ragged input (either monotonically increasing size, monotonically decreasing size, or undulating size)
c. rectangular input (lists same length)
d. one or more lists undefined (partial)
e. one or more lists infinite (testable?)
f. an empty list (should always short-circuit the computation and make the entire result empty).

Small example
given input:
cp [[1, 2, 3, 4], [5, 6], [7, 8, 9]]
expected output:
[[1, 5, 7], [1, 5, 8], [1, 5, 9], [1, 6, 7], [1, 6, 8], [1, 6, 9], ...]
-}
cp :: [[a]] -> [[a]]
cp = foldr addAll []

addAll :: [a] -> [[a]] -> [[a]]
addAll xs xss = if null xss then [[x] | x <- xs] else [x:xs' | x <- xs, xs' <- xss]

cp' :: [[a]] -> [[a]]
cp' = foldr (\xs acc -> [x:ys | x <- xs, ys <- acc]) [[]]
