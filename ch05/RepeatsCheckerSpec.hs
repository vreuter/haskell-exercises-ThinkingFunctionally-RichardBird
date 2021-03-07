-- Tests/specs for checking lists for duplicate items
-- Thinking Functionally with Haskell (Richard Bird)
-- Exercise 5B: given function sort :: [a] -> [a], define nodups :: [a] -> Bool
-- Vince Reuter
-- August 2019

import Test.Hspec
import Test.QuickCheck
import qualified RepeatsChecker
import qualified TestingHelpers

------------------------------------------------------------------------
-- Generators and inputs --
------------------------------------------------------------------------
-- Generate a multi-element List that lacks any element repeats.
multiUniqGen :: (Int -> Gen [a]) -> Gen [a]
multiUniqGen getGen = do
    k <- getSize
    getGen $ max 2 k

-- Generate a List of at least 2 unique integers.
multiUniqInt :: Gen [Int]
multiUniqInt = do
    k <- getSize
    TestingHelpers.uniqueInts (max 2 k)

-- Generate a List of at least 2 unique uppercase English characters.
multiUniqChar :: Gen [Char]
multiUniqChar = do
    k <- getSize
    TestingHelpers.uniqueEnglishUppers (min 26 (max 2 k))

-- Generate a List in which elements are arbitrary, and each is repeated 
-- an arbitrary number of times; ensure that at least one element is 
-- repeated at least once.
randReps :: Gen a -> Gen [a]
randReps g = 
    let expand (x, k) = take k $ repeat x
        getN = max 1
    in (sized $ \n ->
        do xs <- vectorOf (getN n) g
           reps <- suchThat (vectorOf (getN n) (choose (1, 5))) (any (>1))
           (shuffle . concat) $ map expand (xs `zip` reps))
------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    describe "RepeatsChecker.dups" $ do
        it "returns False for empty List" $ do
            {- Specify type for empty list to circumvent ambiguity:
            No instance for (Ord a0)
               arising from a use of ‘RepeatsChecker.dups’
            The type variable ‘a0’ is ambiguous
            -}
            RepeatsChecker.dups ([] :: [Int]) == False
        it "returns False for single-element List" $ do
            forAll (TestingHelpers.justOneElement  :: Gen [String]) $ 
                \s -> length s == 1 && not (RepeatsChecker.dups s)
        it "returns False for multi-element [Int] without duplicates (specificity)" $ do
            forAll multiUniqInt $ \xs -> not $ RepeatsChecker.dups xs
        it "returns False for multi-element [Char] without duplicates (specificity)" $ do
            forAll multiUniqChar $ \cs -> not $ RepeatsChecker.dups cs
        it "return True for List with repe1ats (sensitivity)" $ do
            forAll (randReps (arbitrary :: Gen Int)) $ \xs -> RepeatsChecker.dups xs
    describe "RepeatsChecker.nodups" $ do
        it "returns True for empty List" $ do
            -- Ditto for above on why we need to type the empty List; 
            -- Haskell needs to know that the element type is a member of Ord.
            RepeatsChecker.nodups ([] :: [Int])
        it "returns True for single-element List" $ do
            forAll (TestingHelpers.justOneElement :: Gen [Int]) $ 
                \xs -> length xs == 1 && RepeatsChecker.nodups xs
        it "returns True for all-unique List (sensitivity)" $ do
            forAll multiUniqInt $ \xs -> RepeatsChecker.nodups xs
        it "returns False for List with repeats (specificity)" $ do
            forAll (randReps (arbitrary :: Gen Char)) $ \cs -> not $ RepeatsChecker.nodups cs
    describe "relation between RepeatsChecker.dups and RepeatsChecker.nodups" $ do
        it "is exactly the converse" $ do
            forAll (listOf (arbitrary :: Gen Int)) $ 
                \xs -> RepeatsChecker.dups xs == (not $ RepeatsChecker.nodups xs)
