-- Chapter 4 Exercise F: "Dual" / inverse view of List
-- Thinking Functionally with Haskell (Richard Bird)
-- Vince Reuter
-- June 2019


import Test.QuickCheck

data List a = Nil | Snoc (List a) a deriving (Show, Eq, Ord)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Snoc xs x) = Snoc (fmap f xs) (f x)

head' :: List a -> a
head' Nil = error "empty list"
head' (Snoc Nil x) = x
head' (Snoc xs x) = head' xs

tail' :: List a -> [a]
tail' = tail . fromList

toList :: [a] -> List a
toList = foldl (\acc x -> Snoc acc x) Nil

fromList :: (List a) -> [a]
fromList xs = go xs []
    where go Nil acc = acc
          go (Snoc ys y) acc = go ys (y:acc)

prop_UniversalListRoundtrip xs = (fromList . toList) xs == xs
    where types = (xs::[Int])

prop_NonemptyHeadTail xs = 
    let l = toList xs in (not (null xs) ==> head' l : tail' l == xs)
        where types = (xs::[Char])

