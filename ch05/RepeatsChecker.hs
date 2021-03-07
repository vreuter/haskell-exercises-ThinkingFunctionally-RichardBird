-- Checking lists for duplicate items
-- Thinking Functionally with Haskell (Richard Bird)
-- Exercise 5B: given function sort :: [a] -> [a], define nodups :: [a] -> Bool
-- Vince Reuter
-- August 2019

module RepeatsChecker where

import Data.List    -- for sort :: [a] -> [a]

-- Helper for the exercise's stated objective, a "positive" version
dups :: (Ord a) => [a] -> Bool
dups = go . sort
    where go []     = False
          go (h:[]) = False
          go (h:t@(h':_)) = h == h' || go t

-- Determine whether a list lacks any element-wise repetition.
nodups :: (Ord a) => [a] -> Bool
nodups = not . dups
