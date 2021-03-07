{-
Tests for 5G: Minimum -- reimplement fxn for min val of List
Vince Reuter
August 2019
-}

import qualified Extrema
import Test.Hspec
import Test.QuickCheck

-- Evaluation deferral and exception expectation
import Control.Exception

-- List of at least one randomly generated element
atLeastOne :: Arbitrary a => Gen [a]
atLeastOne = sized $ \n -> do
    k <- choose (1, max 1 n)
    vectorOf k arbitrary

-- Message when minimum of empty List is requested
expEmptyMsg :: String
expEmptyMsg = "Requested min val from empty List"

-- Is the given value the greatest lower bound for the given List?
lowerBounds :: Ord a => a -> [a] -> Bool
lowerBounds x xs = not $ any (<x) xs

main :: IO ()
main = hspec $ do
    describe "Extrema.least" $ do
        it "Gives error with expected message for empty List" $ do
            evaluate (Extrema.least ([] :: [String])) `shouldThrow` errorCall expEmptyMsg
        it "Has no lesser element than result" $ do
            forAll (atLeastOne :: Gen [Int]) $ \xs -> lowerBounds (Extrema.least xs) xs
    describe "Extrema.least'" $ do
        it "Gives error with expected message for empty List" $ do
            evaluate (Extrema.least' ([] :: [Char])) `shouldThrow` errorCall expEmptyMsg
        it "Has no lesser element than result" $ do
            forAll (atLeastOne :: Gen [Char]) $ \cs -> lowerBounds (Extrema.least cs) cs
    describe "Extrema.least and Extrema.least'" $ do
        it "are equivalent w.r.t. inputs and outputs" $ do
            forAll (atLeastOne :: Gen [Int]) $ \xs -> Extrema.least xs == Extrema.least' xs
