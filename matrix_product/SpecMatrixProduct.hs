-- SPECIFICATION for Chapter 5 Exercise A: Matrix operations -- product
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- July 2019

import Test.Hspec
import Test.QuickCheck
import qualified MatrixProduct as MP
import qualified MatrixProductPropositionHelpers as PropHelp
import Test.QuickCheck.Instances.Tuple

-- Matrix paired with its own precomputed dimensions
type MatWithDim a = (MP.Matrix a, (Int, Int))
type MatsWithDims a = (MatWithDim a, MatWithDim a)

-- Determine whether a pair of matrix dimensions are compatible with matrix multiplication.
goodDims :: MatsWithDims a -> Bool
goodDims ((_, dimA), (_, dimB)) = MP.productCompat dimA dimB

-- Generate a pair of matrices, each carrying with it its dimension.
matsWithDims :: PropHelp.ArbNum a => Gen (MatsWithDims a)
matsWithDims = PropHelp.genMatWithDims >*< PropHelp.genMatWithDims

-- Generate a pair of matrices--each carrying dimension--that satisfy some pairwise 
-- predicate regarding the pair of dimensions.
matsWithDimsThat :: PropHelp.ArbNum a => (MatsWithDims a -> Bool) -> Gen (MatsWithDims a)
matsWithDimsThat = suchThat matsWithDims

main :: IO ()
main = hspec $ do
    describe "MatrixProduct.calcMatrProd" $ do
        it "Wraps the dimensions in a Left when dimensions are incompatible" $ do
            forAll (matsWithDimsThat (not . goodDims)) $ \((a, expA), (b, expB)) -> 
                case a `MP.mult` b of Right _ -> False
                                      Left dims -> dims == (expA, expB)
        it "AB has dimension equal rows of A and cols of B" $ do
            forAll (matsWithDimsThat goodDims) $ \((a, dimA), (b, dimB)) -> 
                case a `MP.mult` b of Left _ -> False
                                      Right m -> MP.dimension m == (fst dimA, snd dimB)
        it "AB has as each entry the dot product of x-th row of A and x-th col of B" $ do
            forAll (matsWithDimsThat goodDims) $ \((a, _), (b, _)) -> 
                let b' = MP.rows $ MP.transpose b
                    dot = \u v -> sum $ zipWith (*) u v
                    exp = MP.lift2Mat $ map (\r -> map (dot r) b') $ MP.rows a
                in (case a `MP.mult` b of Left _ -> False
                                          Right m -> m == exp)
