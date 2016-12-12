module Data.Range.Tree.Wrappers
    ( EmptySafe
    ) where

import Data.Range.Tree.Class (RangeTree (..))


-- * Empty-safe tree

newtype EmptySafe t p = EmptySafe (Maybe (t p))

instance Show (t p) => Show (EmptySafe t p) where
    show (EmptySafe Nothing)  = "<empty tree>"
    show (EmptySafe (Just t)) = show t

instance RangeTree t => RangeTree (EmptySafe t) where
    build [] = EmptySafe Nothing
    build ps = EmptySafe . Just $ build ps

    find rs (EmptySafe mt) = maybe [] (find rs) mt


-- * Equal-elements safe tree

-- TODO:
