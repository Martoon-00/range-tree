module Data.Range.Tree
       ( module Data.Range.Tree.Data
       , module Data.Range.Tree.Class
       , module Data.Range.Tree.Smart
       , module Data.Range.Tree.Wrappers
       , Tree
       ) where

import Data.Range.Tree.Class
import Data.Range.Tree.Data
import Data.Range.Tree.Smart
import Data.Range.Tree.Wrappers

type Tree = EmptySafe RawTree
