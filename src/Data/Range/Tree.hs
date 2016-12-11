{-# LANGUAGE TypeFamilies #-}

module Data.Range.Tree
    (
    ) where

import           Control.Lens  (Index, IxValue, Ixed (..), iso)
import qualified Data.Foldable as F
import           Data.Monoid   ((<>))
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

data Tree p = Leaf p
            | Node (Splitter (IxValue p)) (Tree p) (Tree p)
            | Sublayer (Tree p)

instance F.Foldable Tree where
    foldMap f t = doFold t
      where
        doFold (Leaf p)      = f p
        doFold (Node _ l r)  = doFold l <> doFold r
        doFold (Sublayer t') = doFold t'

instance (p ~ Point c) => IsList (Tree p) where
    type Item (Tree p) = p
    fromList = buildTree
    toList = F.toList

buildTree :: [Point c] -> Tree (Point c)
buildTree = undefined
