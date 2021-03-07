{-
Test helpers, mainly generators
Vince Reuter
August 2019
-}

{-# LANGUAGE ConstraintKinds #-}

module TestingHelpers (justOneElement, randomFiniteSansReps, uniqueEnglishUppers, uniqueInts) where

import qualified Data.Hashable as Hash
import qualified Data.HashMap.Strict as SHM
import Test.QuickCheck
import CollectionUtilities (countOccurrences)

-- Alias to constrain type as both hashable checkable for equality
type EqHash a = (Eq a, Hash.Hashable a)

-- Generate a single-element List containing an arbitrary member of a type.
justOneElement :: Arbitrary a => Gen [a]
justOneElement = resize 1 $ listOf1 arbitrary

-- Randomly select (without replacement) a subset of an input List.
randomFiniteSansReps :: EqHash a => Int -> [a] -> Gen [a]
randomFiniteSansReps n pool
    | n < 0  = error ("Requested negative number of elements: " ++ show n)
    | otherwise = let reps = SHM.filter (>1) $ countOccurrences pool
                      avail = SHM.size reps
                  in (if avail < n 
                      then error ("Requested " ++ show n ++ " elements from " ++ show avail)
                      else case SHM.size reps of 0      -> fmap (take n) $ shuffle pool
                                                 numRep -> error (show numRep ++ " repeated keys"))

-- Generator for collection of uppercase letters in which each appears just once.
-- As such, the requested number of characters must not exceed 26, and it must be positive.
uniqueEnglishUppers :: Int -> Gen [Char]
uniqueEnglishUppers n
    | n < 0 || n > 26 =  error ("Number of English characters must be in (0, 26]; got " ++ show n)
    | otherwise = fmap (take n) (shuffle ['A'..'Z'])

-- Gen [Int] in which each element of the result occurs just once.
-- The lone argument is the desired number of elements.
uniqueInts :: Int -> Gen [Int]
uniqueInts n
    | n < 0 = error ("Requested negative number of values: " ++ show n)
    | otherwise = suchThat arbitrary (\xs -> properLength n xs && allUniqElems xs)

-- Test whether each element of a List is never repeated within the List.
allUniqElems :: EqHash a => [a] -> Bool
allUniqElems xs = all (==1) $ (SHM.elems . countOccurrences) xs

-- Check that a collection of objects has a given size/length.
-- The lone argument is the desired size/length to validate.
properLength :: Int -> [a] -> Bool
properLength n = (==n) . length
