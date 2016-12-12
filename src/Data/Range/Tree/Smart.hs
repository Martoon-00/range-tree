{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Data.Range.Tree.Smart
    ( RawTree
    ) where

import           Control.Lens  (ix, makeLensesFor, singular, view, (^.))
import qualified Data.Foldable as F
import           Data.List     (sortBy)
import           Data.Ord      (comparing)
import qualified Data.Vector   as V
import           GHC.Exts      (IsList (..))

import Data.Range.Tree.Class (RangeTree (..))
import Data.Range.Tree.Data  (Belonging (..), Point, Range (..), belong, dimensions,
                              ixUnsafe)


data RawTree p
    = Nil
    | Node
    { _content :: V.Vector p
    , _left    :: RawTree p
    , _right   :: RawTree p
    , _subtree :: RawTree p
    } deriving (Show)
makeLensesFor [("_content", "content")] ''RawTree

instance F.Foldable RawTree where
    foldMap f = foldMap f . _content

instance (p ~ Point c, Ord c) => IsList (RawTree p) where
    type Item (RawTree p) = p
    fromList = buildTree . fromList
    toList = F.toList

getCoordRange :: Int -> RawTree (Point c) -> Range c
getCoordRange i (_content -> c) =
    if V.length c == 0
        then error "getCoordRange: empty tree"
        else Range (V.head c ^. ixUnsafe i)
                   (V.last c ^. ixUnsafe i)

buildTree :: Ord c => V.Vector (Point c) -> RawTree (Point c)
buildTree points =
    if V.length points < 1
        then error "buildTree: can't build for empty set of points"
        else buildTree' 0 points
  where
    buildTree' :: Ord c => Int -> V.Vector (Point c) -> RawTree (Point c)
    buildTree' dim ps
        | V.length ps == 1 = Nil
        | otherwise =
            let (l, r)   = splitByHalf ps
                _left    = buildTree' dim l
                _right   = buildTree' dim r
                _content = merge (comparing getCoord)
                    (_left ^. content) (_right ^. content)
                _subtree = if dim >= maxDim then Nil else buildTree' (dim + 1) ps
            in  Node{..}
      where
        getCoord = view $ singular (ix dim)

    maxDim = dimensions $ V.head points

    -- TODO: redone
    merge cmp v1 v2 = fromList $ sortBy cmp $ toList v1 ++ toList v2


splitByHalf :: V.Vector a -> (V.Vector a, V.Vector a)
splitByHalf v
    | V.length v == 0 = error "splitByHalf: empty vector"
    | otherwise       = (v1, v2)
  where
    i = V.length v `div` 2
    (v1, v2) = V.splitAt i v

findPoints :: Ord c => [Range c] -> RawTree (Point c) -> [Point c]
findPoints = findPoints' 0
  where
    findPoints' :: Ord c => Int -> [Range c] -> RawTree (Point c) -> [Point c]
    findPoints' _   _           Nil      = []
    findPoints' _   []          Node{..} = toList _content
    findPoints' dim rr@(r:rs) t@Node{..} =
        case getCoordRange dim t `belong` r of
            Include  -> findPoints' (dim + 1) rs _subtree
            Partly   -> findPoints' dim rr _left
                     ++ findPoints' dim rr _right
            Disjoint -> []


instance RangeTree RawTree where
    build = buildTree . V.fromList
    find  = findPoints
