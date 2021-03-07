-- Chapter 5 Exercise A: Matrix operations -- product
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- July 2019

module MatrixProduct (
    Matrix, MatrixProduct, 
    calcMatrProd, dimension, lift2Mat, mult, 
    productCompat, rows, transpose) where

import qualified Data.List (transpose)

--------------------------------------------------------
-- Types -----------------------------------------------
--------------------------------------------------------

-- A matrix is built from a list-of-lists.
data Matrix a = Matrix [[a]] deriving (Show, Eq)

-- The product of 2 matrices is either defined and is another matrix, 
-- or it's undefined and the attempted product results in a Left-wrapped 
-- pair of the matrix dimensions.
type MatrixProduct a = Either ((Int, Int), (Int, Int)) (Matrix a)


--------------------------------------------------------
-- Primary Functiona -----------------------------------
--------------------------------------------------------

-- The product of two numeric matrices is either undefined -- in which case 
-- the result is a Left wrapping the dimensions of the pair of operands -- 
-- or the column count in the left operand matches the row count in the 
-- right operand, and hence the matrix product is well defined.
-- 
-- In that ideal case, we iterate over rows of the left operand, taking the 
-- dot product over all pairs of that row with the each column of the right 
-- operand. Transposition facilitates mapping dot product over the right operand's columns.
calcMatrProd :: Num a => Matrix a -> Matrix a -> (MatrixProduct a)
calcMatrProd m1 m2 = if compat d1 d2 then Right (Matrix (prod m1 m2)) else Left (d1, d2)
    where d1 = dimension m1
          d2 = dimension m2
          compat (_, c) (r, _) = c == r
          dot xs ys = sum $ zipWith (*) xs ys
          prod (Matrix m1') m2' = case transpose m2' of Matrix m2t -> [map (dot x) m2t | x <- m1']

-- Obtain the dimension (rows x columns) of the given matrix.
dimension :: Matrix a -> (Int, Int)
dimension (Matrix (r:rs)) = (1 + length rs, length r)

-- "Lift" a list-of-lists into a matrix, verifying rectuangularity along the way.
-- Interpretation is of argument as list of ROWS rather than list of columns.
lift2Mat :: [[a]] -> Matrix a
lift2Mat []     = error "A matrix cannot be empty"
lift2Mat ([]:_) = error "A matrix cannot have an empty row"
lift2Mat rs@(r:rs') = 
    let c = length r
        diffLength = (/= c) . length
    in case filter diffLength rs' of [] -> Matrix rs
                                     _  -> error "Ragged arrays"

-- Alias for matrix multiplication
mult = calcMatrProd

-- Determine if a pair of dimensions is compatible with/for matrix multiplication.
productCompat :: Integral b => (b, b) -> (b, b) -> Bool
productCompat (_, c) (r, _) = c == r

-- Extract the data (stored by row) from the given matrix.
rows :: Matrix a -> [[a]]
rows (Matrix m) = m


-- Transpose a matrix.
transpose :: Matrix a -> Matrix a
transpose (Matrix vs) = Matrix (Data.List.transpose vs)
