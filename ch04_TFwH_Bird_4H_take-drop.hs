-- Chapter 4 Exercise H: Recursive take/drop definitions; splitAt
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

import Test.QuickCheck

myTake :: Int -> [a] -> [a]
myTake n xs
    | n < 0 = error ("Cannot take negative number of elements: " ++ show n)
    | otherwise = go n xs []
        where go 0 _ acc =        acc
              go _ [] acc =       acc
              go n' (x:xs') acc = go (n'-1) xs' (x:acc)

myDrop :: Int -> [a] -> [a]
myDrop n xs
    | n < 0 = error ("Cannot drop negative number of elements: " ++ show n)
    | otherwise = go n xs
        where go 0 xs' = xs'
              go n' (_:xs') = go (n'-1) xs'

-- Traverse the List just once; contrast with:
-- mySplitAt :: Int -> [a] -> ([a], [a])
-- mySplitAt n xs = (take n xs, tail n xs)
mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n xs
    | n < 0 = error ("Cannot split at a negative index: " ++ show n)
    | otherwise = go n xs []
        where go 0 xs' acc =      (reverse acc, xs')
              go _ [] acc =       (reverse acc, [])
              go n' (x:xs') acc = go (n'-1) xs' (x:acc)


-- take k xs ++ drop k xs == xs
prop_TakeDropEquivalence xs = not (null xs) ==> 
    let n = length xs
        takeDropJoin k ys = take k ys ++ drop k ys
    in (do k <- choose (0, maxBound :: Int)
           return (takeDropJoin k xs == xs))


-- fst (mySplitAt i xs) ++ snd (mySplitiAt i xs) == xs
prop_SplitRountrip xs = not (null xs) ==> 
    let n = length xs
        splitJoin k ys = fst p ++ snd p
            where p = mySplitAt k ys
    in (do k <- choose (0, maxBound :: Int)
           return ((splitJoin k xs) == xs))
