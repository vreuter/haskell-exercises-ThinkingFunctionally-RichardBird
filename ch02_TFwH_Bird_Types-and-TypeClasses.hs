-- Chapter 2 Exercises: Types and Type Classes
-- Thinking Functionall with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019

import Data.Char
import Data.Maybe


-- Exercise C: journal paper title capitalization assurance
-- Ensure that the title of a journal article is title-cased.
upperFirst :: String -> String
upperFirst [] = []
upperFirst s = (toUpper (head s)) : (tail s)

modernise :: String -> String
modernise t = unwords (map upperFirst (words t))


-- Exercise E: safer implementation of find-first-satisfactory operation
-- That is, implement a function that finds the first element of a list that 
-- satisfies some predicate, returning a Maybe as the result.
first :: (a -> Bool) -> [a] -> Maybe a
first _ [] =  Nothing
first f (x:xs) = if f x then Just x else first f xs


-- Exercise F: exponentiation implementation
exp' :: Integer -> Integer -> Integer
exp' x n | n == 0 = 1
         | n == 1 = x
         | otherwise = x * (exp' x (n - 1))

exp'' :: Integer -> Integer -> Integer
exp'' x n | n == 0 = 1
          | n == 1 = x
          | even n = (exp'' x (n `div` 2))^2
          | odd n  = x * (exp'' x ((n-1) `div` 2))^2


-- Exercise G: Textifying a date
-- Input: (day, month, year)
-- Output: <ordinal> <month>, <year>
-- Ordinals should have suffix (th, st, etc.)
months = ["January", "February", "March", "April", 
          "May", "June", "July", "August", 
          "September", "October", "November", "December"]

numDays :: Integer -> String -> Int
numDays y m | m `elem` ["January", "March", "May", "July", "August", "October", "December"] = 31
            | m `elem` ["April", "June", "September", "November"] = 30
            | m == "February" = if (y `mod` 4 == 0) && (y `mod` 100 /= 0 || y `mod` 400 == 0) then 29 else 28

type Day = Int
type Month = Int
type Year = Integer

mText :: Int -> String
mText i = months !! (i - 1)

daySuffix :: Int -> String
daySuffix d | d < 1 || d > 31 = error "Invalid month date: " ++ show d
            | d `elem` [4..20] || d `elem` [24..30] = "th"
            | ones == 1 = "st"
            | ones == 2 = "nd"
            | ones == 3 = "rd"
                where ones = d `mod` 10

type Date = (Day, Month, Year)
showDate :: Date -> String
showDate (d, m, y) | d < 1 = "Invalid day: " ++ show d
                   | m < 1 || m > 12 = "Invalid month: " ++ show m
                   | otherwise = if d > nd 
                                 then show d ++ " is not a valid date for " ++ mText m ++ " " ++ show y
                                 else show d ++ daySuffix d ++ " " ++ mText m ++ ", " ++ show y
                                     where nd = numDays y (mText m)


-- Exercise H: card identification numbers (CINs)
-- Produce 10-digit CIN with 2-digit checksum suffix from 8-digit CIN without checksum.
type CIN = String

addSum :: CIN -> CIN
addSum n | length n /= 8 = error "Card number must be 8 digits"
         | otherwise = n ++ show (sum (map digitToInt n))

valid :: CIN -> Bool
valid wholeCardNum = 
    let digitCount = 8 
        digits = splitAt digitCount wholeCardNum
        cn = fst digits
        cs = snd digits
    in (length cn == digitCount && sum (map digitToInt cn) == (read cs :: Int))
