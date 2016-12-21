{-# LANGUAGE TemplateHaskell #-}

module Test.Commons
    ( -- * Point modifiers
      OrderedPoint (..)
    , ArbitraryPoint (..)
      -- * Type numerals
    , Zero
    , Succ
    , Numeral (..)
    , ordinal
    , withDim
    -- * Request
    , Request (..)
    ) where

import           Data.Function       (on)
import           Data.Ord            (comparing)
import qualified Data.Vector         as V
import           Language.Haskell.TH (Q, Type (..))
import           Test.QuickCheck     (Arbitrary (..), Gen, vectorOf)

import Data.Range.Tree (Point (..), Range (..))


-- * Ordered Point

newtype OrderedPoint c = OrderedPoint (Point c)

instance Show c => Show (OrderedPoint c) where
    show (OrderedPoint p) = show p

instance Eq c => Eq (OrderedPoint c) where
    (==) = (==) `on` (\(OrderedPoint (Point v)) -> v)

instance Ord c => Ord (OrderedPoint c) where
    compare = comparing (\(OrderedPoint (Point v)) -> v)


-- * Numbers in types

data Zero
data Succ n

-- | Generates given type-number.
-- For example,
-- @
-- $(ordinal 3)
-- @
--
-- transforms to
--
-- @
-- Succ (Succ (Succ Zero))
-- @
ordinal :: Int -> Q Type
ordinal k = return $ foldr AppT (PromotedT ''Zero) $ replicate k (PromotedT ''Succ)


class Numeral n where
    numeral :: n -> Int

instance Numeral Zero where
    numeral _ = 0

instance Numeral n => Numeral (Succ n) where
    numeral s = numeral (numPred s) + 1
      where
        numPred :: Succ n -> n
        numPred = const undefined

newtype ArbitraryPoint d c = ArbitraryPoint (Point c)

instance Show c => Show (ArbitraryPoint d c) where
    show (ArbitraryPoint p) = show p

instance (Arbitrary c, Numeral d) => Arbitrary (ArbitraryPoint d c) where
    arbitrary = withDim $ \n -> ArbitraryPoint . Point . V.fromList
            <$> vectorOf (numeral n) arbitrary

withDim :: (d -> Gen (t d c)) -> Gen (t d c)
withDim = ($ undefined)

instance (Arbitrary c, Ord c) => Arbitrary (Range c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Range (min a b) (max a b)

newtype Request d c = Request [Range c]

instance Show c => Show (Request d c) where
    show (Request rs) = show rs

instance (Arbitrary c, Ord c, Numeral d) => Arbitrary (Request d c) where
    arbitrary = withDim $ \n -> Request <$> vectorOf (numeral n) arbitrary
