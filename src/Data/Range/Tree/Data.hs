{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Data.Range.Tree.Data
    ( Point
    , dimensions

    , PathChoice
    , Splitter

    , Range (..)
    , fit
    , pointFit
    ) where


import           Control.Lens (Index, IxValue, Ixed (..), iso, singular, (^.))
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


-- * Splitter

data PathChoice = LeftPath
                | RightPath
    deriving (Enum)

type Splitter p = p -> PathChoice

instance Show (Splitter p) where
    show _ = "<splitter>"


-- * Range

newtype Range c = Range (c, c)

fit :: Ord c => Range c -> c -> Bool
fit (Range (l, r)) x = l <= x && x <= r

pointFit :: Ord c => [Range c] -> Point c -> Bool
pointFit rs p = if length rs /= dimensions p
               then error "pointFit: Number of ranges and dimensions siffer!"
               else all (\(i, r) -> r `fit` (p ^. singular (ix i))) (zip [0..] rs)
