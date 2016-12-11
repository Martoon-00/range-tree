module Data.Range.Tree.Class
    ( RangeTree (..)
    ) where

import Data.Range.Tree.Data (Point, Range, pointFit)

class RangeTree t where
    build :: Ord c => [Point c] -> t (Point c)

    find :: Ord c => [Range c] -> t (Point c) -> [Point c]

instance RangeTree [] where
    build = id
    find = filter . pointFit
