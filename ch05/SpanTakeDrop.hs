{-
span, takeWhile, dropWhile
Thinking Functionally with Haskell (Richard Bird)
Exercise 5F: dropWhile and takeWhile implementation
Vince Reuter
August 2019
-}

module SpanTakeDrop where

import Text.Ascii (isWhiteSpace)

-- Reimplementation of takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = let halt         = not . p
                     go [] acc    = acc
                     go (h:t) acc = if halt h then acc else go t (h:acc)
                 in reverse $ go xs []

-- Reimplementation of dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = go p xs
    where go _ [] = []
          go p l@(h:t) = if p h then go p t else l

-- Assume isWhiteSpace :: Char -> Bool
words' :: String -> [String]
words' s = reverse $ go s []
    where go [] acc = acc
          go s acc = let (word, rest) = span (not . isWhiteSpace) s
                         update w ws  = if null w then ws else (w:ws)
                     in go (Prelude.dropWhile isWhiteSpace rest) $ update word acc
