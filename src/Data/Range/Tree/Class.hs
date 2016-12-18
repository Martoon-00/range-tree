module Data.Range.Tree.Class
    ( RangeTree (..)
    ) where

import Data.Range.Tree.Data (Point, Range, pointFit)

-- | Interface of range tree
class RangeTree t where
    -- | Construct tree from given point list
    build :: Ord c => [Point c] -> t (Point c)

    -- | Search for points in given multidimentional range
    find :: Ord c => [Range c] -> t (Point c) -> [Point c]

-- | Dummy implementation with search for O(n), used in tests as ethalon
instance RangeTree [] where
    build = id
    find = filter . pointFit
