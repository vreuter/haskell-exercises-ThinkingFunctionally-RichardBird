-- Chapter 5 Exercise A: Matrix operations
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

import Data.Bifunctor                     -- bimap
import Data.Either                        -- isLeft, isRight, etc.
import Test.QuickCheck                    -- forAll, ==>, etc.
import Test.QuickCheck.Instances.Tuple    -- >*< (Gen a -> Gen b -> (Gen (a, b)))


--------------------------------------------------------
-- Types -----------------------------------------------
--------------------------------------------------------

type Row a = [a]
type Matrix a = [Row a]
type FirstBadIndex = Either Int Int
type AddRowRes a = Either (Row a, FirstBadIndex) (Row a)

data MatDim = MatDim Int Int deriving Eq
instance Show MatDim where
    show (MatDim r c) = show r ++ "x" ++ show c


-----------------------
-- Primary functions --
-----------------------

-- Vector sum, basic/simple implementation
addRows :: Num a => Row a -> Row a -> Row a
addRows = zipWith (+)

-- Vector sum with assurance of length match
addRowsMatch :: Num a => Row a -> Row a -> AddRowRes a
-- In either outcome case/"side", reverse the list (i.e., whether it's a partial or full result).
-- In the Left side, preserve the index as-is; the integral value stored in the Either is the 
-- index of the first position in one row without a counterpart in the other (i.e., ) the 
-- length of the shorter of the 2 rows. The case of the Either it's wrapped in indicates 
-- which input row was shorter.
addRowsMatch r1 r2 = bimap (\(r, i) -> (reverse r, i)) reverse (go r1 r2 0 (Right []))
    -- Once we've hit a Left, we're stuck (row length mismatch)
    where go _ _ _ (Left res) = Left res
          -- If either row is empty, we've arrived at a result; it's Right iff both are exhausted.
          go [] [] _ (Right res) = Right res
          go [] _ i (Right acc)  = Left (acc, Left i)
          go _ [] i (Right acc)  = Left (acc, Right i)
          -- Increment index and recurse on list tails, consing sum onto accumulator.
          go (x:xs) (y:ys) i (Right acc) = go xs ys (i+1) (Right ((x+y):acc))

-- Sum a pair of matrices.
addMats :: Num a => Matrix a -> Matrix a -> Matrix a
addMats = zipWith addRows

-- Increment by a fixed amount each entry in a matrix.
incrementEntries :: Num a => a -> Matrix a -> Matrix a
incrementEntries x = map (map (+x))
--------------------------------------------------------------------------------------


--------------------------------------------------
-- Row-/vector-wise generators and propositions --
--------------------------------------------------

--prop_RowsSumComponentWise :: Row a -> Row a -> Bool
prop_RowsSumComponentWise r1 r2 = [x + y | (x, y) <- r1 `zip` r2] == addRows r1 r2
    where types = (r1 :: Row Int, r2 :: Row Int)

-- Randomly generate a pair of same-length rows.
genFlushRows :: Num a => Gen a -> Gen (Row a, Row a)
genFlushRows g = sized $ \n ->
    do k <- choose (0, n)
       r1 <- vectorOf k g
       r2 <- vectorOf k g
       return (r1, r2)

-- Randomly generate a list of elements that's arbitrarily shorter than the given list.
genShorter :: Arbitrary a => [a] -> Gen [a]
genShorter [] = error "Cannot generate shorter than empty list."
genShorter (_:xs) = do k <- choose (0, length xs)
                       vectorOf k arbitrary

-- For pair of rows that differ in length, sum results in a Left-wrapped value.
prop_RaggedRowsSumToALeft r1 r2 = length r1 /= length r2 ==> isLeft $ addRowsMatch r1 r2

-- For pair of rows that match in length, sum result in a Right-wrapped value.
prop_FlushRowsSumToARight r1 = forAll (vectorOf (length r1) arbitrary) $ \r2 -> isRight $ addRowsMatch r1 r2

-- For pair of rows that differ in length, the size of the smaller row which row is smaller are correct.
prop_ShorterRowCorrect r1 = not (null r1) ==> 
    forAll (genShorter r1) $ \r2 -> 
        -- r2 is shorter than r1, so the assertion must hold iff:
        -- 1. The row sum is a Left (indicating failure / partial result)
        -- 2. The first-problematic-index is the length of the shorter row
        -- 3. The smaller row is correctly identified (communicated by the particular instance of the Either; 
        --    a Right-wrapped index here is expected is r2 is shorter and is the right-hand operand).
        case addRowsMatch r1 r2 of Right _ -> False
                                   Left (_, Left _) -> False
                                   Left (_, Right i) -> i == length r2
----------------------------------------------------------------------


-----------------------------------------------------
-- Matrix generation, properties, and propositions --
-----------------------------------------------------

-- Determine whether each list is the same size.
sameSizeN :: Integral a => a -> [[b]] -> Bool
sameSizeN n = and . map ((==n) . fromIntegral . length)

-- Randomly generate an arbitrarily large square matrix.
genSquareMatrix :: Gen (Matrix Int)
genSquareMatrix = sized $ \n -> genSquareMatrixN n

-- Randomly generate a square matrix of given row/column count.
genSquareMatrixN :: (Arbitrary a, Num a) => Int -> Gen (Matrix a)
genSquareMatrixN n
    | n < 0 = error ("Requested negative dimension for square matrix: " ++ show n)
    | otherwise = vectorOf n (vectorOf n arbitrary)

-- Generate a pair of identically sized square matrices.
genSquareMatrixPair :: (Arbitrary a, Num a) => Gen (Matrix a, Matrix a)
genSquareMatrixPair = sized $ \n -> 
    do k <- choose (0, n)
       m1 <- genSquareMatrixN n
       m2 <- genSquareMatrixN n
       return (m1, m2)

-- Check whether a matrix is square.
-- A matrix is squar if it's empty/null or if each's row's size equals the number of rows.
isSquare :: [[a]] -> Bool
isSquare m = case length m of 0 -> True
                              r -> (and . map (== r)) (map length m)

-- Any allegedly square matrix generated is in fact square.
prop_SquareIsSquare = forAll genSquareMatrix $ \m -> isSquare m

-- Generate matrix with at least a certain number of rows and columns.
-- 1st argument is inclusive lower bound on row count.
-- 2nd argument is inclusive lower bound on column count.
genMatMinSize :: (Arbitrary a, Num a) => Int -> Int -> Gen (Matrix a)
genMatMinSize minR minC
    | minR < 0 || minC < 0 = error ("Negative lower bound(s) on matrix dims: " ++ (show (MatDim minR minC)))
    | otherwise = sized $ \n -> do r <- choose (minR, n)
                                   c <- choose (minC, n)
                                   genMatFixedSize r c

-- Randomly generate a numeric matrix of a particular, fixed size.
-- 1st argument is row count and 2nd argument is column count.
-- Both row count and column count must be nonnegative.
genMatFixedSize :: (Arbitrary a, Num a) => Int -> Int -> Gen (Matrix a)
genMatFixedSize r c
    | r < 0 || c < 0 = error ("Negative matrix dims request: " ++ (show (MatDim r c)))
    | otherwise =      vectorOf r (vectorOf c arbitrary)

-- Randomly generate arbitrarily sized numeric matrix, and include its dimensions.
genMatWithDimsNonempty :: (Arbitrary a, Num a) => Gen (MatDim, Matrix a)
genMatWithDimsNonempty = sized $ \n -> 
    do r <- choose (1, max n 1)
       c <- choose (1, max n 1)
       m <- genMatFixedSize r c
       return (MatDim r c, m)

-- Verify expected dimensions of a randomly generated, arbitrarily sized numeric matrix.
prop_GenMatFixedSizeCarriesCorrectDimensions = 
    forAll (genMatWithDimsNonempty :: Gen (MatDim, Matrix Int)) $ \(MatDim r c, m) -> 
        if null m then r == 0 && c == 0 else length m == r && length (head m) == c && sameSizeN c m

-- Nonempty matrix generation never generates empty matrix.
prop_GenMatWithDimsNonemptyIsNonempty = 
    forAll (genMatWithDimsNonempty :: Gen (MatDim, Matrix Int)) $ \(_, m) -> 
        not (null m || null (head m))

-- Randomly generate a pair of numeric matrices with the same dimension.
genMatPairSameDims :: (Arbitrary a, Num a) => Gen (Matrix a, Matrix a)
genMatPairSameDims = sized $ \n -> 
    do r <- choose (0, n)
       c <- choose (0, n)
       (genMatFixedSize r c) >*< (genMatFixedSize r c)

-- Two matrices have the same dimension iff both are empty or both have same number 
-- of rows, the first rows match in length, and both are rectangular.
sameDims :: Matrix a -> Matrix a -> Bool
sameDims [] [] = True
sameDims [] _ = False
sameDims _ [] = False
sameDims m@(r:rs) m'@(r':rs') = 
    (length m == length m') && (c == length r') && rect rs && rect rs'
        where c = length r
              rect = sameSizeN (fromIntegral c)

eqvFlatSum :: (Eq a, Num a) => Matrix a -> Matrix a -> Bool
eqvFlatSum m1 m2 = zipWith (+) (concat m1) (concat m2) == concat (addMats m1 m2)

-- Check that any pair of allegedly same-sized square matrices do match on size.
prop_SquareMatrixPairIsSameSize = 
    forAll (genSquareMatrixPair :: Gen (Matrix Int, Matrix Int)) $ \(m1, m2) -> sameDims m1 m2

-- Check that the matrices in any pair of matrices (square or not) with allegedly same dims 
-- do in fact match with respect to dimension.
prop_SameSizeMatrixPairIsSameSize = 
    forAll (genMatPairSameDims :: Gen (Matrix Int, Matrix Int)) $ \(m1, m2) -> sameDims m1 m2

-- BUGGY!
-- Attempt assertion that summing flattened matrices is equivalent to flattening their sum.
-- This is FALSE (the matrix generation process is insufficiently constrained for the 
-- implicit universal quantification to hold / be satisfied.)
prop_FlattenMatrixSum m1 m2 = zipWith (+) (concat m1) (concat m2) == concat (addMats m1 m2)
    where types = (m1 :: Matrix Int, m2 :: Matrix Int)

-- FIXED!
-- Randomly generate a pair of square matrices with matching dimension, and check 
-- the sum flattening equivalence only over restricted domain of matrix pairs.
-- This is TRUE (i.e., flattened sum formulations of square matrices of the same size are equivalent.)
prop_MatchedDimsSquareMatrixSum = 
    forAll (genSquareMatrixPair :: Gen (Matrix Int, Matrix Int)) $ \(m1, m2) -> eqvFlatSum m1 m2

-- Proposition of equivalence between matrix sum formulations, with matrices restricted to match in dimension.
-- This is TRUE (i.e., matrix dimension match --> flattened sum formulations match.)
-- NOTE: this is a more general result than the square matrix proposition (prop_MatchedDimsSquareMatrixSum).
prop_FlatSumSameDimsMats = 
    forAll (genMatPairSameDims :: Gen (Matrix Int, Matrix Int)) $ \(m1, m2) -> eqvFlatSum m1 m2
-----------------------------------------------------------------------------------------------


---------------------------------------------------------------------
-- Matrix pairs in which row counts match but column counts differ --
---------------------------------------------------------------------
-- Generate matrices that match on column count but differ in row count.
genDiffRowSameColMatrices :: (Arbitrary a, Num a) => Gen (Matrix a, Matrix a)
genDiffRowSameColMatrices = sized $ \n ->
    let maxR = max n 2
    in do c <- choose (1, max n 1)                        -- Diff row count but same col count precludes empty matrix.
          r1 <- choose (1, maxR)                          -- Allow "space" for a different r2.
          r2 <- suchThat (choose (1, maxR)) (/= r1)
          m1 <- genMatFixedSize r1 c
          m2 <- genMatFixedSize r2 c
          return (m1, m2)

-- Validate shape/dimension properties of generation of pairs of matrices in 
-- which the row counts should differ but the column counts should match.
prop_DiffRowSameColMatPairGenIsCorrect = 
    noShrinking $ forAll (genDiffRowSameColMatrices :: Gen (Matrix Int, Matrix Int)) $ \(m1, m2) -> 
        case (m1, m2) of ([], _) -> False
                         (_, []) -> False
                         _       -> (length m1 /= length m2) && 
                                    (let c1 = fromIntegral(length(head(m1)))
                                     in (sameSizeN c1 m1) && (sameSizeN c1 m2))

-- So long as column counts match, flattened sum formulations are equivalent.
-- This is TRUE: the "lesser-list" rule for zipping applies, leading the 
-- flattened sums to be equivalent since either order of the truncation of 
-- the longer list still results in a vector with the first (R x C) entries.
prop_DiffRowSameColMatricesHaveEquivalentFlattenedSums = 
    forAll (genDiffRowSameColMatrices :: Gen (Matrix Int, Matrix Int)) $ \(m1, m2) -> eqvFlatSum m1 m2
------------------------------------------------------------------------------------------------------


---------------------------------------------------------------------
-- Matrix pairs in which row counts match but column counts differ --
---------------------------------------------------------------------

-- Randomly generate pair of matrices with same row count but different column count.
genSameRowDiffColMatrices :: (Arbitrary a, Num a) => Gen (Matrix a, Matrix a)
genSameRowDiffColMatrices= sized $ \n ->
    let maxC = max n 2 
    in do r <- choose (1, max n 1)
          c1 <- choose (1, maxC)
          c2 <- suchThat (choose (1, maxC)) (/= c1)
          m1 <- genMatFixedSize r c1
          m2 <- genMatFixedSize r c2
          return (m1, m2)

-- Validate random generation of pairs of matrices with same row count and different 
-- column count.
prop_SameRowDiffColGenIsCorrect = 
    forAll (genSameRowDiffColMatrices :: Gen (Matrix Int, Matrix Int)) $ \(m1, m2) -> 
        case (m1, m2) of ([], _) -> False
                         (_, []) -> False
                         _       -> let nc = length . head
                                        (c1, c2) = (nc m1, nc m2)
                                    in c1 /= c2 && sameSizeN c1 m1 && sameSizeN c2 m2
