-- SPECIFICATION for Chapter 5 Exercise A: Matrix operations -- dimension
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- July 2019

import Test.Hspec
import Test.QuickCheck
import qualified MatrixProduct as MP (dimension, Matrix)
import qualified MatrixProductPropositionHelpers as PropHelp


main :: IO ()
main = hspec $ do
    describe "MatrixProduct.dimension" $ do
        it "Is accurate" $ do 
            forAll (PropHelp.genMatWithDims :: Gen (MP.Matrix Int, (Int, Int))) $ 
                \(m, d) -> d == MP.dimension m
