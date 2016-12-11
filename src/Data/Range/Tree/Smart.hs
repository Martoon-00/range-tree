{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Range.Tree.Smart
    ( Tree
    ) where

import           Control.Lens  (ix, makeLensesFor, singular, view, (^.))
import           Control.Monad (guard)
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.List     (sortBy)
import           Data.Ord      (comparing)
import qualified Data.Vector   as V
import           GHC.Exts      (IsList (..))

import Data.Range.Tree.Class (RangeTree (..))
import Data.Range.Tree.Data  (Point, Range, Splitter, dimensions)


data Tree p
    = Nil
    | Node
    { _splitter :: Splitter p
    , _content  :: V.Vector p
    , _left     :: Tree p
    , _right    :: Tree p
    , _subtree  :: Maybe (Tree p)
    } deriving (Show)
makeLensesFor [("_content", "content")] ''Tree

instance F.Foldable Tree where
    foldMap f = foldMap f . _content

instance (p ~ Point c, Ord c) => IsList (Tree p) where
    type Item (Tree p) = p
    fromList = buildTree . fromList
    toList = F.toList

buildTree :: Ord c => V.Vector (Point c) -> Tree (Point c)
buildTree points =
    if V.length points < 2
        then error "Can't build for lengths 0 & 1"
        else buildTree' 0 points
  where
    buildTree' :: Ord c => Int -> V.Vector (Point c) -> Tree (Point c)
    buildTree' dim ps
        | V.length ps == 1 = Nil
        | otherwise =
            let (l, r, _splitter) = splitByHalf getCoord ps
                _left    = buildTree' dim l
                _right   = buildTree' dim r
                _content = merge (comparing getCoord)
                    (_left ^. content) (_right ^. content)
                _subtree = guard (dim < maxDim) >> return (buildTree' (dim + 1) ps)
            in  Node{..}
      where
        getCoord = view $ singular (ix dim)

    maxDim = dimensions $ V.head points

    -- TODO: redone
    merge cmp v1 v2 = fromList $ sortBy cmp $ toList v1 ++ toList v2


splitByHalf :: Ord c => (a -> c) -> V.Vector a -> (V.Vector a, V.Vector a, Splitter a)
splitByHalf ext v
    | V.length v == 0 = error "splitByHalf applied to empty vector"
    | otherwise       = (v1, v2, toEnum . fromEnum . ((>=) `on` ext) (V.head v2))
  where
    i = V.length v `div` 2
    (v1, v2) = V.splitAt i v

findPoints :: Ord c => [Range c] -> Tree (Point c) -> [Point c]
findPoints = undefined

instance RangeTree Tree where
    build = buildTree . V.fromList
    find  = findPoints
