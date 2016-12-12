module Test.GeneralSpec
    ( spec
    ) where

import           Data.Function   (on)
import           Data.Ord        (comparing)
import           Data.Proxy
import qualified Data.Set        as S
import qualified Data.Vector     as V
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, property, vectorOf,
                                  (===))

import Data.Range.Tree (Point (..), Range (..), RangeTree (..), Tree)

spec :: Spec
spec =
    describe "General" $
        it "simple" $
            property $ generalTest (Proxy :: Proxy (Succ Zero))

generalTest :: Proxy n -> [ArbitraryPoint Double n] -> Request Double n -> Property
generalTest _ origin (Request test) =
    let origin'  = map (\(ArbitraryPoint p) -> p) origin
        ans  = find test $ (build origin' :: Tree (Point Double))
        nice = find test $ (build origin' :: [Point Double])
    in  S.fromList (OrderedPoint <$> ans)
    === S.fromList (OrderedPoint <$> nice)


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

class Numeral n where
    numeral :: n -> Int

instance Numeral Zero where
    numeral _ = 0

instance Numeral n => Numeral (Succ n) where
    numeral s = numeral (numPred s) + 1
      where
        numPred :: Succ n -> n
        numPred = const undefined

newtype ArbitraryPoint c n = ArbitraryPoint (Point c)

instance Show c => Show (ArbitraryPoint c n) where
    show (ArbitraryPoint p) = show p

instance (Arbitrary c, Numeral n) => Arbitrary (ArbitraryPoint c n) where
    arbitrary = withDim $ \n -> ArbitraryPoint . Point . V.fromList
            <$> vectorOf (numeral n) arbitrary
      where
        withDim :: (n -> Gen (ArbitraryPoint c n)) -> Gen (ArbitraryPoint c n)
        withDim = ($ undefined)


instance (Arbitrary c, Ord c) => Arbitrary (Range c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Range (min a b) (max a b)

newtype Request c n = Request [Range c]

instance Show c => Show (Request c n) where
    show (Request rs) = show rs

instance (Arbitrary c, Ord c, Numeral n) => Arbitrary (Request c n) where
    arbitrary = withDim $ \n -> Request <$> vectorOf (numeral n) arbitrary
      where
        withDim :: (n -> Gen (Request c n)) -> Gen (Request c n)
        withDim = ($ undefined)
