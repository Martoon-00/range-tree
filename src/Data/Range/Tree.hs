module Data.Range.Tree
       ( module Data.Range.Tree.Data
       , module Data.Range.Tree.Class
       , module Data.Range.Tree.Raw
       , module Data.Range.Tree.Wrappers
       , Tree
       ) where

import Data.Range.Tree.Class
import Data.Range.Tree.Data
import Data.Range.Tree.Raw
import Data.Range.Tree.Wrappers

-- | Full working implementation of range-tree algorithm
type Tree = EmptySafe RawTree
