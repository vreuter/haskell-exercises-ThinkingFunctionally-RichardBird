{-
Tests for test helpers (mainly generators)
Vince Reuter
August 2019
-}

import TestingHelpers (justOneElement)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "TestingHelpers.justOneElement" $ do
        it "always generates a List of a single element" $ do
            forAll (justOneElement :: Gen [Int]) $ \xs -> 1 == length xs
