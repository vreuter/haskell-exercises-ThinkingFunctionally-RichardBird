-- Chapter 4 Random Code Snippets: Lists
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ concat xss

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x):(myMap f xs)

-- Usual/typical strategy for filter implementation
-- Encodes notion of "evaluate condition on head, and if satisfied then prepend to result of recursion."
-- Includes the element itself in the accumulation (of intermediate values in memory)
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs) = if p x then x:(myFilter p xs) else myFilter p xs

-- Alternative strategy for filter implementation
-- Encodes the notion of "lift-then-flatten."
-- Includes the container-wrapped element (e.g., singleton list) 
-- in the accumulation (of intermediate values in memory)
myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' pred = concat . map (test pred)
    where test :: (a -> Bool) -> a -> [a]
          test p x = if p x then [x] else []


-- Implement each (sub)tree as either an endpoint ("leaf" <--> Tip) or a "branch" point (Fork)
data Tree a = Tip a | Fork (Tree a) (Tree a)
instance Functor Tree where
    fmap f (Tip x) = Tip (f x)
    fmap f (Fork y z) = Fork (fmap f y) (fmap f z)


-- First index where predicate is true, -1 if it's never true
index :: (a -> Bool) -> [a] -> Integer
index p xs = head ([i | (i, x) <- zip [1..] xs, p x] ++ [-1])
