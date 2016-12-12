{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.Range.Tree.Data
    ( Point (..)
    , dimensions

    , Range (..)
    , fit
    , pointFit
    , belong
    , Belonging (..)

    , ixUnsafe
    ) where


import           Control.Lens (Index, IxValue, Ixed (..), Lens', iso, singular, (^.))
import qualified Data.Vector  as V

-- ** Point

newtype Point c = Point
    { coords :: V.Vector c
    } deriving (Show)

instance Ixed (Point c) where
    ix i = iso coords Point . ix i

type instance IxValue (Point c) = c
type instance Index (Point c) = Int

dimensions :: Point c -> Int
dimensions = V.length . coords

ixUnsafe :: Ixed m => Index m -> Lens' m (IxValue m)
ixUnsafe = singular . ix


-- * Splitter

data PathChoice = LeftPath
                | RightPath
    deriving (Enum)

type Splitter p = p -> PathChoice

instance Show (Splitter p) where
    show _ = "<splitter>"


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

belong :: Ord c => Range c -> Range c -> Belonging
belong (Range l r) (Range l' r')
    | l >= l' && r <= r' = Include
    | l >  r' || r < l'  = Disjoint
    | otherwise          = Partly
