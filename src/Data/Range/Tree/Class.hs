module Data.Range.Tree.Class
    ( RangeTree (..)
    ) where

import qualified Data.DList  as DL
import           Data.Monoid (Monoid)
import qualified Data.Vector as V

import Data.Range.Tree.Data (Point, Range, pointFit)

-- | Interface of range tree
class RangeTree t where
    -- | Construct tree from given point list
    build :: Ord c => [Point c] -> t (Point c)

    -- | Search for points in given multidimentional range
    findFold :: (Monoid m, Ord c)
             => t (Point c) -> (V.Vector (Point c) -> m) -> [Range c] -> m

    -- | Search for points in given multidimentional range
    find :: Ord c => t (Point c) -> [Range c] -> [Point c]
    find t rs = DL.toList $ findFold t (DL.fromList . V.toList) rs


-- | Dummy implementation with search for O(n), used in tests as ethalon
instance RangeTree [] where
    build = id
    findFold t f rs = foldMap f $ map V.singleton $ filter (pointFit rs) t
