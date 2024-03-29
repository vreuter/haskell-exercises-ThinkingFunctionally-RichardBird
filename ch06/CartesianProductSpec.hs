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

newtype CartProd a = CartProd { getNameAndFunction :: (String, [[a]] -> [[a]]) }

extractName :: CartProd a -> String
extractName = fst . getNameAndFunction

computeProduct :: CartProd a -> [[a]] -> [[a]]
computeProduct = snd . getNameAndFunction

instance Show (CartProd a) where
    show = extractName

randomProductFunction :: Gen (CartProd a)
randomProductFunction = CartProd <$> elements [("cp", CP.cp), ("cp'", CP.cp')]

genFuncArgPair :: Gen [[a]] -> Gen (CartProd a, [[a]])
genFuncArgPair genArg = (,) <$> randomProductFunction <*> genArg

main :: IO ()
main = hspec $ do
    describe "CartesianProduct.cp" $ do
        it "is empty list for empty list input" $ do
            null $ CP.cp ([] :: [[Int]])
        it "is empty list IF (-->) any input component is empty" $ do
            forAll (genFuncArgPair (minOneEmpty :: Gen [[Char]])) $ null . uncurry computeProduct
        it "for nonempty input, result is empty ONLY IF (<--) an input component is empty" $ do 
            {-
            Note: this is an EXISTENCE proof ("empty result implies EXISTENCE of an empty input component")
            -}
            forAll (genFuncArgPair (allNonempty :: Gen [[Int]])) $ not . null . uncurry computeProduct
        it "yields lists in which each element has length equal to number of original lists" $ do
            forAll (genFuncArgPair (allNonempty :: Gen [[Int]])) $ \(cartProd, xss) -> 
                let nonempty  = (not . null)
                    n         = length xss
                    allExpLen = all ((==n) . length)
                in nonempty xss && allExpLen (computeProduct cartProd xss)
        it "has size equal to the product of the sizes of the inputs" $ do
            forAll (genFuncArgPair (allNonempty :: Gen [[Int]])) $ \(cartProd, xss) -> 
                length (computeProduct cartProd xss) == (product . map length) xss
        it "preserves order" $ do
            forAll (genFuncArgPair (sortedAndParted :: Gen [[Int]])) $ \(cartProd, xss) -> 
                let res = computeProduct cartProd xss 
                in res == map Data.List.sort res
        it "draws exactly one element from each input sublist, in correct order" $ do
            forAll (genFuncArgPair (sortPartG $ sublistOf [-10..10])) $ \(cartProd, xss) -> 
                let oneOcc x = (==[x]) . filter (==x)
                    sublists = xss `zip` [0..]
                    check xs = all (\(x, i) -> all (\(xs', i') -> if i == i' then oneOcc x xs' else x `notElem` xs') sublists) (xs `zip` [0..])
                in all check $ computeProduct cartProd xss
