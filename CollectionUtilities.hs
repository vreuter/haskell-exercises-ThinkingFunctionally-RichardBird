{-
Collections utilities
Vince Reuter
August 2019
-}

module CollectionUtilities where

import qualified Data.HashMap.Strict as SHM
import qualified Data.Hashable as Hash

{-
Count the number of times that each element appears in a List.
The input is a List of elements, which must be of a type that's 
both hashable and testable for equality. The output is a mapping 
from element to occurrence count.
-}
countOccurrences :: (Eq k, Hash.Hashable k) => [k] -> SHM.HashMap k Int
countOccurrences ks = go ks SHM.empty
    where incr key = SHM.insertWith (+) key 1
          go [] m    = m
          go (h:t) m = go t (incr h m)
