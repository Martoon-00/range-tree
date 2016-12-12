module Data.Range.Tree.Wrappers
    ( EmptySafe
    ) where

import Data.Range.Tree.Class (RangeTree (..))
import Data.Range.Tree.Smart (RawTree)


-- * Empty-safe tree

newtype EmptySafe t p = EmptySafe (Maybe (t p))

instance RangeTree t => RangeTree (EmptySafe t) where
    build [] = EmptySafe Nothing
    build ps = EmptySafe . Just $ build ps

    find rs (EmptySafe mt) = maybe [] (find rs) mt


-- * Equal-elements safe tree

-- TODO:
