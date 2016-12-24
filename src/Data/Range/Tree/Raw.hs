{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Data.Range.Tree.Raw
    ( RawTree
    ) where

import           Control.DeepSeq  (NFData)
import           Control.Lens     ((^.))
import qualified Data.Foldable    as F
import           Data.List        (intersperse)
import qualified Data.List        as L
import           Data.Monoid      (mempty, (<>))
import           Data.Ord         (comparing)
import           Data.Traversable (mapAccumL)
import qualified Data.Vector      as V
import           GHC.Exts         (IsList (..))
import           GHC.Generics     (Generic)

import Data.Range.Tree.Class (RangeTree (..))
import Data.Range.Tree.Data  (Belonging (..), Point, Range (..), belong, coord,
                              dimensions, ixUnsafe)


-- | Implementation of range-tree algorithm
data RawTree p
    = Nil
    | Node
    { _content :: V.Vector p
    , _left    :: RawTree p
    , _right   :: RawTree p
    , _subtree :: RawTree p
    , _refs    :: V.Vector Int
    , _ends    :: (p, p)
    } deriving (Generic)

instance Show p => Show (RawTree p) where
    show t = "tree\n" ++ show' 2 t
      where
        show' d Nil      = replicate d ' ' ++ "-"
        show' d Node{..} = concat . intersperse "\n" $
            [ replicate d ' ' ++ show _content
            , replicate d ' ' ++ "left ="
            , show' (d + 2) _left
            , replicate d ' ' ++ "right ="
            , show' (d + 2) _right
            , replicate d ' ' ++ "sub ="
            , show' (d + 2) _subtree
            ]

instance F.Foldable RawTree where
    foldMap f = foldMap f . _content

instance (p ~ Point c, Ord c) => IsList (RawTree p) where
    type Item (RawTree p) = p
    fromList = buildTree . fromList
    toList = F.toList

instance NFData p => NFData (RawTree p)

asForList :: ([a] -> [b]) -> V.Vector a -> V.Vector b
asForList f = V.fromList . f . V.toList

-- | Range in which coordinate at given dimension varies
getCoordRange :: Ord c => Int -> RawTree (Point c) -> Range c
getCoordRange i (_ends -> (l, r)) = Range (coord i l) (coord i r)

comparingBackward :: Point c -> Point c -> Ordering
comparingBackward = undefined

buildTree :: Ord c => V.Vector (Point c) -> RawTree (Point c)
buildTree points =
    if V.length points < 1
        then -- we don't know dimension of tree, so throw error
             error "buildTree: can't build for empty set of points"
        else buildTree' 0 points V.empty
  where
    buildTree' :: Ord c
               => Int -> V.Vector (Point c) -> V.Vector (Point c) -> RawTree (Point c)
    buildTree' dim ps contentAbove
        | V.length ps == 1 =
            let _content = ps
                _left    = Nil
                _right   = Nil
                _subtree = mkSubTree _content
                _refs    = mkRefs contentAbove _content
                _ends    = mkEnds
            in  Node{..}
        | otherwise =
            let _content = asForList (L.sortBy . comparingBackward) ps
                (l, r)   = splitByHalf _content
                _left    = buildTree' dim l _content
                _right   = buildTree' dim r _content
                _subtree = mkSubTree _content
                _refs    = mkRefs contentAbove _content
                _ends    = mkEnds
            in  Node{..}
      where
        mkSubTree ca = if dim < maxDim
                       then buildTree' (dim + 1) ps ca
                       else Node
                        { _content = ps
                        , _left    = Nil
                        , _right   = Nil
                        , _subtree = Nil
                        , _refs    = mkRefs contentAbove ca
                        , _ends    = mkEnds
                        }

        mkEnds = (,) <$> F.minimumBy (comparing $ coord dim)
                     <*> F.maximumBy (comparing $ coord dim) $ ps


    maxDim = dimensions (V.head points) - 1

    -- for each value in contentAbove get minimal index of element in contentHere,
    -- which is equal or more than value, comparing by last coordinate.
    mkRefs contentAbove contentHere =
        snd $ mapAccumL f (0,  V.toList contentHere) contentAbove
      where f (k, [])        _ = ((k, []), k)
            f (k, hh@(h:hs)) a
                | h == a    = ((k,     hh), k    )
                | otherwise = ((k + 1, hs), k + 1)

{-
-- | Merges two sorted vectors to sorted vector, using provided comparator
mergeBy :: (c -> c -> Ordering) -> V.Vector c -> V.Vector c -> V.Vector c
mergeBy cmp v1 v2 = fromList $ doMerge (toList v1) (toList v2)
  where
    doMerge [] ys = ys
    doMerge xs [] = xs
    doMerge xs@(x:xr) ys@(y:yr)
        | cmp x y == LT = x : doMerge xr ys
        | otherwise     = y : doMerge xs yr
-}

splitByHalf :: V.Vector a -> (V.Vector a, V.Vector a)
splitByHalf v
    | V.length v == 0 = error "splitByHalf: empty vector"
    | otherwise       = (v1, v2)
  where
    i = V.length v `div` 2
    (v1, v2) = V.splitAt i v

findPoints :: (Monoid m, Ord c)
           => RawTree (Point c) -> (V.Vector (Point c) -> m) -> [Range c] -> m
findPoints t' f rs' = findPoints' 0 rs' t'
  where
    findPoints' _   _           Nil      = mempty
    findPoints' _   []          Node{..} = f _content
    findPoints' dim rr@(r:rs) t@Node{..} =
        case getCoordRange dim t `belong` r of
            Include  -> findPoints' (dim + 1) rs _subtree
            Partly   -> findPoints' dim rr _left
                     <> findPoints' dim rr _right
            Disjoint -> mempty

instance RangeTree RawTree where
    build = buildTree . V.fromList
    findFold = findPoints
