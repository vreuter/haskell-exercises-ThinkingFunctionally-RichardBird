-- Chapter 4 Exercise E: Numbers as distinct sums of cubes
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

-- Sums of cubes
cubeSumsNaive :: Int -> [((Integer, Integer), (Integer, Integer))]
cubeSumsNaive n
    | n < 1 = []
    | otherwise = take n [((a, d), (b, c)) | d <- [1..], c <- [1..(d-1)], b <- [1..c], a <- [1..(b-1)], a^3 + d^3 == b^3 + c^3]
