{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.Range.Tree.Data
    ( Point (..)
    , dimensions
    , coord

    , Range (..)
    , fit
    , pointFit
    , belong
    , Belonging (..)

    , ixUnsafe
    ) where


import           Control.DeepSeq (NFData (..))
import           Control.Lens    (Index, IxValue, Ixed (..), Lens', iso, singular, view,
                                  (^.))
import qualified Data.Vector     as V


-- ** Point

newtype Point c = Point
    { coords :: V.Vector c
    } deriving (Show)

instance Ixed (Point c) where
    ix i = iso coords Point . ix i

type instance IxValue (Point c) = c
type instance Index (Point c) = Int

instance NFData c => NFData (Point c) where
    rnf = rnf . coords

dimensions :: Point c -> Int
dimensions = V.length . coords

ixUnsafe :: Ixed m => Index m -> Lens' m (IxValue m)
ixUnsafe = singular . ix

coord :: Int -> Point c -> c
coord k = view . singular $ ix k


-- * Range

data Range c = Range c c
    deriving (Show)

fit :: Ord c => Range c -> c -> Bool
fit (Range l r) x = l <= x && x <= r

pointFit :: Ord c => [Range c] -> Point c -> Bool
pointFit rs p = if length rs /= dimensions p
               then error "pointFit: Number of ranges and dimensions differ!"
               else all (\(i, r) -> r `fit` (p ^. singular (ix i))) (zip [0..] rs)

data Belonging
    = Include
    | Partly
    | Disjoint
    deriving (Show)

-- | Whether first range is inside of second
belong :: Ord c => Range c -> Range c -> Belonging
belong (Range l r) (Range l' r')
    | l >= l' && r <= r' = Include
    | l >  r' || r < l'  = Disjoint
    | otherwise          = Partly
