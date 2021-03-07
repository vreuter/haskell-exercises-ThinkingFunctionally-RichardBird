-- Chapter 3 Exercises: Numbers
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

-- Exercise B: Implementing ^^ (AN exponention operation)
-- carats: raise a Fractional to any Integral power
carats :: (Fractional a, Integral b) => a -> b -> a
carats x y = if y < 0 then 1/(x^(negate y)) else x^y

-- Exercise E: Integer Square Root in log time
-- Implement integral square root (using truncation) in time proportional to log of input.
-- Input: a nonnegative floating-point number
-- Output: a nonnegative integer: the floor of the square root of the input
-- Strategy: square the floor and ceiling, then "squeeze out" the solution by halving the interval.
isqrt :: Float -> Integer
isqrt x = fst $ until (\(a,b) -> b - a == 1) (shrink x) (bound x) 

-- Interval constructed to contain some value x;  a <= x < b
type Interval = (Integer, Integer)

-- Shrink a containment interval so that it still contains the desired value.
shrink :: Float -> Interval -> Interval
shrink x (a,b) = if ((k^2) `leq` x) then (k,b) else (a,k)
    where k = (a + b) `div` 2

-- Construct an interval so that contains the given value.
bound :: Float -> Interval
bound x = (0, go 2)
    where go b = let sq = b^2
                 in (if sq `gt` x then b else go sq)

-- <= operation, but lifting the constraint on operand type match
-- Accomodate comparison of integral and real.
-- Integral facilitates conversion to general numeric, but we want Real to use Ord.
leq :: (Integral a, Real b) => a -> b -> Bool
leq x y = (fromInteger . toInteger) x <= y

-- > operation, but lifting the constraint on operand type match
-- Accomodate comparison of integral and real.
-- Integral facilitates conversion to general numeric, but we want Real to use Ord.
gt :: (Integral a, Real b) => a -> b -> Bool
gt x y = not (x `leq` y)


-- Exercise F: Newton's Method -- square root approximation
-- y and x/y approximate sqrt(x).
-- y <= sqrt(x) <= x/y OR x/y <= sqrt(x) <= y
-- Better: TAKE AVERAGE of the bounding interval -- (y + x/y)/2 even better approximates sqrt(x).
apxSqrt :: RealFrac a => (a -> a -> a) -> a -> a -> a
apxSqrt getErr tol x 
    | x < 0 = error ("Cannot use Newton's sqrt approximation for a negative value: ~ " ++ show (round x))
    | x == 0 = 0
    | otherwise = go 1
        where go y = if getErr y x <= tol then y else go ((y + x/y)/2)

apxSqrtAbs :: RealFrac a => a -> a -> a
apxSqrtAbs = apxSqrt (\g x -> abs (g^2 - x))

apxSqrtRel :: RealFrac a => a -> a -> a
apxSqrtRel relTol x = apxSqrtAbs (relTol * x) x

real2Frac :: (Real a, Fractional b) => a -> b
real2Frac = (fromRational . toRational)


-- Exercise G: ordering natural numbers
-- Provide an Ord instance for Nat.
-- The provision of Ord for Nat --> divMod implementation acc. to natural succession of Nat.
data Nat = Zero | Succ Nat
instance Show Nat where
    show Zero = "Zero"
    show (Succ Zero) = "Succ Zero"
    show (Succ (Succ n)) = "Succ (" ++ show (Succ n) ++ ")"
instance Eq Nat where
    Zero == Zero = True
    Zero == (Succ _) = False
    (Succ _) == Zero = False
    (Succ a) == (Succ b) = a == b
instance Num Nat where
    a + Zero = a
    a + Succ b =  Succ (a + b)
    a * Zero = Zero
    a * Succ b = a * b + a
    abs n = n
    signum Zero = Zero
    signum (Succ _) = Succ Zero
    a - Zero = a
    Zero - (Succ _) = Zero
    Succ a - Succ b = a - b
    fromInteger n = if n <= 0 then Zero else Succ (fromInteger (n-1))
instance Ord Nat where
    compare Zero Zero = EQ
    compare Zero (Succ _) = LT
    compare (Succ _) Zero = GT
    compare (Succ a) (Succ b) = compare a b

-- divModNat: quotient and remainder for natural number arguments
-- Input: dividend and divisor
-- Output: quotient and remainder
divModNat :: Nat -> Nat -> (Nat, Nat)
divModNat x n = go Zero Zero
    where go q y = if y < x then go (q + Succ Zero) (y + n) else if y > x then (q - Succ Zero, y - x) else (q, Zero)
