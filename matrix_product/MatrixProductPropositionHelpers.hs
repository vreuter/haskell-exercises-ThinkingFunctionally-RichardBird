-- Matrix product proposition helpers
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- July 2019

{-# LANGUAGE ConstraintKinds #-}

module MatrixProductPropositionHelpers where

import Test.QuickCheck
import qualified MatrixProduct as MP

----------------------------------------------
-- Typelevel stuff
----------------------------------------------
-- Constrain type universe to types with instances of Arbitrary and Num.
type ArbNum a = (Arbitrary a, Num a)
------------------------------------------------------------------------

------------------------------------------------------------------------
-- Generators and inputs --
------------------------------------------------------------------------
-- Nonemty rectangular array of numerics constitutes matrix.
genMatWithDims :: ArbNum a => Gen (MP.Matrix a, (Int, Int))
genMatWithDims = do
    r <- choose (1, 10)
    c <- choose (1, 10)
    m <- vectorOf r $ vectorOf c arbitrary
    return (MP.lift2Mat m, (r, c))

-- Generate a nonempty, numeric matrix.
genNonemptyMat :: ArbNum a => Gen (MP.Matrix a)
genNonemptyMat = fmap fst genMatWithDims
------------------------------------------------------------------------
