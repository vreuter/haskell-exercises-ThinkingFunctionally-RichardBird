-- QuickCheck properties for list comprehensions expression evaluation order demonstration
-- Basis: Exercise 4D from "Thinking Functionally with Haskell" (Richard Bird -- 2015)
-- Vince Reuter
-- June 2019

import Test.QuickCheck

listCompPairPredFirstElem :: (a -> Bool) -> [a] -> [b] -> [(a, b)]
listCompPairPredFirstElem p xs ys = [(x, y) | x <- xs, p x, y <- ys]

-- undefined needs explicit type annotation!
{-|
prop_AlwaysShortCircuit xs = listCompPairPredFirstElem (\_ -> False) xs undefined == []
    where types = (xs::[Int])

*Main> :l ch04_TFwH_Bird_ListCompEval_QuickCheck.hs 
[1 of 1] Compiling Main             ( ch04_TFwH_Bird_ListCompEval_QuickCheck.hs, interpreted )

ch04_TFwH_Bird_ListCompEval_QuickCheck.hs:11:83:
    No instance for (Eq b0) arising from a use of ‘==’
    The type variable ‘b0’ is ambiguous
    Note: there are several potential instances:
      instance Eq a => Eq (Control.Applicative.Const a b)
        -- Defined in ‘Control.Applicative’
      instance Eq a => Eq (Control.Applicative.ZipList a)
        -- Defined in ‘Control.Applicative’
      instance Eq a => Eq (Data.Complex.Complex a)
        -- Defined in ‘Data.Complex’
      ...plus 95 others
    In the expression:
      listCompPairPredFirstElem (\ _ -> False) xs undefined == []
    In an equation for ‘prop_AlwaysShortCircuit’:
        prop_AlwaysShortCircuit xs
          = listCompPairPredFirstElem (\ _ -> False) xs undefined == []
          where
              types = (xs :: [Int])
Failed, modules loaded: none.
-}

prop_AlwaysShortCircuit cs = listCompPairPredFirstElem (\_ -> False) cs (undefined :: [Char]) == []
    where types = (cs::[Char])

prop_NonzeroShortCircuit xs = not (any (==0) xs) ==> 
    listCompPairPredFirstElem (==0) xs (undefined :: [Int]) == []
        where types = (xs::[Int])
