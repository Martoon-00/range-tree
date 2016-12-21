module Data.Range.Tree.Wrappers
    ( EmptySafe
    ) where

import Data.Range.Tree.Class (RangeTree (..))
import Control.DeepSeq (NFData (..))


-- * Empty-safe tree

-- | This implementation considers empty tree as special case,
-- while non-empty sets are gelegated to underlying data structure `t`.
newtype EmptySafe t p = EmptySafe (Maybe (t p))

instance Show (t p) => Show (EmptySafe t p) where
    show (EmptySafe Nothing)  = "<empty tree>"
    show (EmptySafe (Just t)) = show t

instance RangeTree t => RangeTree (EmptySafe t) where
    build [] = EmptySafe Nothing
    build ps = EmptySafe . Just $ build ps

    find (EmptySafe mt) rs = maybe [] (flip find rs) mt

instance NFData (t p) => NFData (EmptySafe t p) where
    rnf (EmptySafe t) = rnf t
