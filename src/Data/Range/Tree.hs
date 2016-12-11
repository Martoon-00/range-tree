{-# LANGUAGE TypeFamilies #-}

module Data.Range.Tree
    (
    ) where

import           Control.Lens  (Index, IxValue, Ixed (..), iso)
import qualified Data.Foldable as F
import           Data.Vector   (Vector)
import           GHC.Exts      (IsList (..))

-- * Datatypes

-- ** Point

newtype Point c = Point
    { coords :: Vector c
    } deriving (Show)

instance Ixed (Point c) where
    ix i = iso coords Point . ix i

type instance IxValue (Point c) = c
type instance Index (Point c) = Int


-- * Splitter

data PathChoice = Left | Right

type Splitter c = Point c -> PathChoice


-- * Tree

data Tree p
    = Nil
    | Node
    { splitter :: (Splitter (IxValue p))
    , content  :: (Vector p)
    , left     :: (Tree p)
    , right    :: (Tree p)
    , subtree  :: Maybe (Tree p)
    }

instance F.Foldable Tree where
    foldMap f = foldMap f . content

instance (p ~ Point c) => IsList (Tree p) where
    type Item (Tree p) = p
    fromList = buildTree
    toList = F.toList

buildTree :: [Point c] -> Tree (Point c)
buildTree = undefined
