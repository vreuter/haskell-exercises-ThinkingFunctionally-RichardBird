{-
Tests/specs for Cartesian product and related functions (6D), 
Thinking Functionally with Haskell (Richard Bird)

cp :: [[a]] => [[a]]

Vince Reuter
August 2019
-}

import qualified Data.List
import Test.Hspec
import Test.QuickCheck
import qualified CartesianProduct as CP

{-
Generate list of at least one element, ensuring that 
among contained lists, at least 1 is empty.
-}
minOneEmpty :: Arbitrary a => Gen [[a]]
minOneEmpty = sized $ \n -> 
    let lol = listOf1 (listOf arbitrary)
        g   = resize (min 1 n) lol
    in suchThat g (any null)

{-
Generate list of at least one element, ensuring that 
among contained lists, all are nonempty.
-}
allNonempty :: Arbitrary a => Gen [[a]]
allNonempty = sized $ \n -> 
    let innerListN = (ceiling . sqrt . fromIntegral) n
    in nonemptySized n $ listOf1 $ nonemptySized innerListN (listOf1 arbitrary)

-- Resize a generator such that it's nonempty.
nonemptySized :: Int -> Gen a -> Gen a
nonemptySized n = resize (min 1 n)

{-
Generate a list-of-lists of arbitrary elements, in which elements are sorted and 
then partitioned at random points.
-}
sortedAndParted :: (Arbitrary a, Ord a) => Gen [[a]]
sortedAndParted = sortPartG $ listOf arbitrary

{-
Generate a list-of-lists of arbitrary elements, in which elements are sorted and 
then partitioned at random points.
-}
sortPartG :: Ord a => Gen [a] -> Gen [[a]]
sortPartG g = sized $ \n -> do
    elems <- fmap Data.List.sort $ resize (min 50 $ max 1 n) g
    let n' = length elems - 1
    k <- choose (0, max 0 n')
    is <- fmap Data.List.sort $ sublistOf [1..n']
    return (splitAtIndexes elems is)

{-
Split at list into sublists at various indices.
-}
splitAtIndexes :: [a] -> [Int] -> [[a]]
splitAtIndexes [] _ = []
splitAtIndexes xs [] = [xs]
splitAtIndexes xs is = reverse $ map reverse $ go (Data.List.sort is) 0 xs [] []
    where go [] _ _ _ acc     = acc
          go _ _ [] _ acc     = acc
          go is'@(i':is) i rest@(x:xs) acc' acc = if i' == i 
                                         then go is i rest [] (acc':acc)
                                         else go is' (i + 1) xs (x:acc') acc

main :: IO ()
main = hspec $ do
    describe "CartesianProduct" $ do
        it "is empty list for empty list input" $ do
            null $ CP.cp ([] :: [[Int]])
        it "is empty list IF (-->) any input component is empty" $ do
            forAll (minOneEmpty :: Gen [[Char]]) $ \xss -> null $ CP.cp xss
        it "for nonempty input, result is empty ONLY IF (<--) an input component is empty" $ do 
            {-
            Note: this is an EXISTENCE proof ("empty result implies EXISTENCE of an empty input component")
            In other words, the quantification is existential rather than universal.
            -}
            forAll (allNonempty :: Gen [[Int]]) $ \xss -> (not . null) $ CP.cp xss
        it "yields lists in which each element has length equal to number of original lists" $ do
            forAll (allNonempty :: Gen [[Int]]) $ \xss -> 
                let nonempty  = (not . null)
                    n         = length xss
                    allExpLen = all ((==n) . length)
                in nonempty xss && (allExpLen $ CP.cp xss)
        it "has size equal to the product of the sizes of the inputs" $ do
            forAll (allNonempty :: Gen [[Int]]) $ \xss -> length (CP.cp xss) == (product . map length) xss
        it "preserves order" $ do
            forAll (sortedAndParted :: Gen [[Int]]) $ \xss -> 
                let res = CP.cp xss in res == map Data.List.sort res
        it "draws exactly one element from each input sublist, in correct order" $ do
            forAll (sortPartG $ sublistOf [-10..10]) $ \xss -> 
                let oneOcc x = (==[x]) . filter (==x)
                    sublists = xss `zip` [0..]
                    check xs = all (\(x, i) -> all (\(xs', i') -> if i == i' then oneOcc x xs' else not (x `elem` xs')) sublists) (xs `zip` [0..])
                in all check $ CP.cp xss
