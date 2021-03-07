-- Chapter 4 Random Snippet: Comparing Order of Expression Evaluation in List Comprehension
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

-- (Attempt to) pair elements before evaluating predicate (independent of 2nd element.)
-- Hits error (Prelude.undefined)
listCompPairFirst :: (a -> Bool) -> [a] -> [b] -> [(a, b)]
listCompPairFirst p xs ys = [(x, y) | x <- xs, y <- ys, p x]

-- Hits error (Prelude.undefined)
listCompZipPredFirst :: (a -> Bool) -> [a] -> [b] -> [(a, b)]
listCompZipPredFirst p xs ys = [(x, y) | (x, y) <- xs `zip` ys, p x]

-- Evaluate predicate (independent of 2nd element) before pairing. 
-- Avoids undefined/"bottom" error in some cases (predicate universally false over finite first list).
listCompPredFirst :: (a -> Bool) -> [a] -> [b] -> [(a, b)]
listCompPredFirst p xs ys = [(x, y) | x <- xs, p x, y <- ys]

