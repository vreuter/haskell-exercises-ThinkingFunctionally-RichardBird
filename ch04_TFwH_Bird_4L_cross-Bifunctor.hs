-- Chapter 4 Exercise L: The "cross" "plumbing" combinator; Bifunctor
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

import Data.Bifunctor

-- fork applies a pair of functions to a single value, 
-- obtaining a pair of outputs from a single input.
--
-- This is a "plumbing" combinator in Prelude, useful 
-- for "point-free" style proofs regarding definitions.
fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)

-- cross applies a pair of functions to a paired input, 
-- obtaining a pair of outputs from a pair of inputs.
--
-- This is a "plumbing" combinator in Prelude, useful 
-- for "point-free" style proofs regarding definitions.
cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = fork (f . fst, g . snd)


-- Generalise cross as Bifunctor
--class Bifunctor p where
    -- Notice: the  type signature of bimap conjures that of cross, 
    -- just "wrapping" the pairs of elements in the Bifunctor container.
    -- The key difference is that the functions to apply to an input 
    -- are given individually rather than as a pair / 2-tuple; this 
    -- accords with the generalisation of the "container" type from 
    -- a standard 2-tuple to a Bifunctor.
--    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

type Pair a b = (a, b)

crossPair :: (a -> c, b -> d) -> Pair a b -> Pair c d
crossPair (f, g) p = bimap f g p

-- Reimplement standard Either.
data MyEither a b = MyL a | MyR b

-- Make reimplemented Either a member of Bifunctor.
instance Bifunctor MyEither where
    bimap f _ (MyL a) = MyL (f a)
    bimap _ g (MyR b) = MyR (g b)
