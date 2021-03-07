-- Palindrome.hs
-- An interactive terminal "game" that determines whether input is a palindrome.
-- In the palindrome arbitration, ignore whitespace, punctuation, and casing.

import Data.Char (isAlpha, toLower)

homogenizeTextInput :: String -> String
homogenizeTextInput [] = []
homogenizeTextInput (c:cs) = 
    if isAlpha c then toLower c : homogenizeTextInput cs else homogenizeTextInput cs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome items = test (items `zip` (reverse items))
    where test [] = True
          test ((x,y):xs) = x == y && test xs

main = do {
    putStrLn "Enter a string:";
    line <- getLine;
    if isPalindrome $ homogenizeTextInput line
    then do
        putStrLn "Yes!"
    else do
        putStrLn "No!"
}
