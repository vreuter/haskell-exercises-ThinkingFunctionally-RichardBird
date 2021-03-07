{-
Tests for span/takeWhile/dropWhile implementation(s) and relations
Motivated by Thinking Functionally with Haskell, Exercise 5F
Vince Reuter
August 2019
-}

import qualified SpanTakeDrop as TestMod
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple    -- For >*< combinator (Gen of 2-tuple from 2 Gen's)

-- Rnadomized integer predicate based on randomized divisibility disjunction
randDivModIntPred :: Gen (Int -> Bool)
randDivModIntPred = do
    divs <- suchThat (sublistOf [2, 3, 5, 7]) (not . null)
    return (\a -> any (\b -> a `mod` b == 0) divs)

-- List of at least one randomly generated element
atLeastOne :: Arbitrary a => Gen [a]
atLeastOne = sized $ \n -> do
    k <- choose (1, max 1 n)
    vectorOf k arbitrary

-- Randomize elements to drop from List head, other elements, and drop arbitration predicate.
setupDropGen :: Gen ([Char], [Char], Char -> Bool)
setupDropGen = 
    let alpha = ['a'..'z'] ++ ['A'..'Z']
        alphaSize = length alpha
    {-
    1. Randomize size of pool of characters to designate as passing predicate; at least 1, at least 1 not
    2. Randomize number of characters to place at head of list (and eventually drop)
    3. Randomize the characters to designate as passing predicate.
    4. Randomize the characters to place at head of List (drawing from among those passing predicate).
    5. Randomize the character that will halt the discard process.
    6. Randomize the other remaining tail characters (rest of the intended result).
    7. Return the elements to discard, the expected result, and the predicate.
    -}
    in (sized $ \n -> do k <- choose (1, min (alphaSize - 1) $ max 1 (n - 1))    -- 
                         k' <- choose (1, 5)          -- Number of characters to place at head of List
                         predChars <- vectorOf k $ elements alpha    -- 
                         headChars <- vectorOf k' $ elements predChars
                         haltChar <- elements (filter (\c -> not (c `elem` predChars)) alpha)
                         otherTailChars <- vectorOf (n - k' - 1) $ elements alpha
                         return (headChars, (haltChar:otherTailChars), (`elem` predChars)))

{- DOES NOT COMPILE
instance Show (Char -> Bool) where
    show _ = "Char -> Bool"
-}

{- To appease QuickCheck/runhaskell, provide a Show instance for a 
   1-arg function since it's part of a randomly generated tuple. -}
instance Show (a -> b) where
    show _ = "1-arg fxn"

main :: IO ()
main = hspec $ do
    describe "SpanTakeDrop.dropWhile" $ do
        {- 3 cases w.r.t. input
        1. empty
        2. nonempty, 1st element fails predicate  --> test lack of drops
        3. nonempty, 1st element passes predicate --> test drop accuracy
        -}
        it "Returns empty List for empty List" $ do
            null (TestMod.dropWhile (\_ -> False) []) && null (TestMod.dropWhile (\_ -> True) [])
        it "Returns input List if it's nonempty and first element fails predicate" $ do
            forAll ((listOf arbitrary) :: Gen [Int]) $ \xs -> xs == TestMod.dropWhile (\_ -> False) xs
        it "Drops from input List head exactly the elements that pass the predicate" $ do
            forAll setupDropGen $ \(drops, keeps, pred) -> 
                all pred drops && keeps == TestMod.dropWhile pred (drops ++ keeps)
    describe "SpanTakeDrop.takeWhile" $ do
        {- 3 cases w.r.t. input
        1. empty
        2. nonempty, 1st element fails predicate  --> test lack of take (empty result)
        3. nonempty, 1st element passes predicate --> test accuracy of take (exact elements)
        -}
        it "Returns empty List for empty List" $ do
            null (TestMod.takeWhile (\_ -> False) []) && null (TestMod.takeWhile (\_ -> True) [])
        it "Is empty if the first element fails predicate" $ do
            forAll (atLeastOne :: Gen [Int]) $ \xs -> null $ TestMod.takeWhile (\_ -> False) xs
        it "For nonempty input with valid first element, result is first contiguous block of valid elements" $ do
            forAll setupDropGen $ \(keeps, extra, pred) -> 
                all pred keeps && keeps == TestMod.takeWhile pred (keeps ++ extra)
    describe "equivalence with standard counterparts" $ do
        it "holds for dropWhile" $ do
            forAll ((listOf $ choose (0, maxBound :: Int)) >*< randDivModIntPred) $ 
                \(xs, p) -> dropWhile p xs == TestMod.dropWhile p xs
        it "holds for takeWhile" $ do
            let ls = ['a'..'z'] ++ ['A'..'Z']
            forAll ((listOf $ elements ls) >*< (sublistOf ls)) $ 
                \(cs, p) -> let p = (\c -> c `elem` cs) 
                            in takeWhile p cs == TestMod.takeWhile p cs
    describe "Prelude.words and SpanTakeDrop.words'" $ do
        it "are exactly equivalent with respect to input/output" $ do
            forAll arbitrary $ \s -> words s == TestMod.words' s
