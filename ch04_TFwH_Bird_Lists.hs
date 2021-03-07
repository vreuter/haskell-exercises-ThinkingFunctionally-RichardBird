-- Chapter 4 Exercises: Lists
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

import Data.Maybe

disjoint :: Ord a => [a] -> [a] -> Bool
disjoint xs@(x1:x2) ys@(y1:y2)
    | x1 < y1 = disjoint (dropWhile (<y1) x2) ys
    | x1 > y1 = disjoint xs (dropWhile (<x1) y2)
    | otherwise = False
disjoint _ _ = True

disjoint' :: Ord a => [a] -> [a] -> Bool
disjoint' xs ys = isNothing $ findFirstShared xs ys

findFirstShared :: (Ord a) => [a] -> [a] -> Maybe a
findFirstShared xs@(xh:xt) ys@(yh:yt)
    | xh < yh = findFirstShared (dropWhile (<yh) xt) yt
    | xh > yh = findFirstShared xs (dropWhile (<xh) yt)
    | otherwise = Just xh
findFirstShared _ _ = Nothing

difference :: (Ord a) => [a] -> [a] -> [a]
difference xs ys = go xs ys []
    where go [] r acc = acc ++ r
          go r [] acc = acc ++ r
          go x@(xh:xt) y@(yh:yt) acc
              | xh < yh = let (novelX, remX) = span (<yh) x
                          in (go remX y (acc ++ novelX))
              | xh > yh = let (novelY, remY) = span (<xh) y 
                          in (go x remY (acc ++ novelY))
              | otherwise = go xt yt acc
