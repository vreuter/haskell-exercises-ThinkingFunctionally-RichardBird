-- SPECIFICATION for Chapter 5 Exercise A: Matrix operations -- transpose
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- July 2019

import Test.Hspec
import Test.QuickCheck
import qualified MatrixProduct as MP
import qualified MatrixProductPropositionHelpers as PropHelp


main :: IO ()
main = hspec $ do
    describe "MatrixProduct.transpose" $ do
        it "Flips dimensions" $ do 
            forAll (PropHelp.genMatWithDims :: Gen (MP.Matrix Int, (Int, Int))) $ 
                \(m, (r, c)) -> (MP.dimension . MP.transpose) m == (c, r)
        it "Is an operation with order 2" $ do
            forAll (PropHelp.genNonemptyMat :: Gen (MP.Matrix Int)) $ \m -> 
                let idViaT = MP.transpose . MP.transpose
                in m == idViaT m
