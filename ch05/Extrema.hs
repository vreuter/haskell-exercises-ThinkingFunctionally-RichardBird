{-
5G: Minimum -- reimplement fxn for min val of List
More generally, set up a module here for functions related to collection extrema.
Vince Reuter
August 2019
-}

module Extrema (least, least') where

-- The least element of a List, i.e. a greatest/maximal lower bound.
-- DIRECT recursive implementation (not a fold)
least :: Ord a => [a] -> a
least []    = errorEmpty "min val"
least (h:t)  = go h t
    where go curr [] = curr
          go curr (h:t) = go (if h < curr then h else curr) t

-- The least element of a List, i.e. a greatest/maximal lower bound.
-- INDIRECT recursive implementation (foldl)
least' :: Ord a => [a] -> a
least' [] = errorEmpty "min val"
least' (h:t) = foldl min h t

-- Error with a message with particular context
errorEmpty :: String -> a
errorEmpty msgCtx = error $ "Requested " ++ msgCtx ++ " from empty List"
