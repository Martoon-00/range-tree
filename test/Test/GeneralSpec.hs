module Test.GeneralSpec
    ( spec
    ) where

import           Data.Function   (on)
import           Data.List       (sort)
import           Data.Ord        (comparing)
import           Data.Proxy
import qualified Data.Vector     as V
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, Small, property,
                                  vectorOf, whenFail, (===))

import Data.Range.Tree (Point (..), Range (..), RangeTree (..), Tree)


spec :: Spec
spec =
    describe "General" $ do
        describe "Double" $
            dimensional (Proxy :: Proxy Double)
        describe "Int" $
            dimensional (Proxy :: Proxy Int)

        -- check for processing of equal points
        describe "Small int" $
            dimensional (Proxy :: Proxy (Small Int))
        describe "Bool" $
            dimensional (Proxy :: Proxy Bool)

  where
    dimensional p = do
        it "1D" $
            property $ generalTest p (Proxy :: Proxy One)
        it "2D" $
            property $ generalTest p (Proxy :: Proxy Two)
        it "3D" $
            property $ generalTest p (Proxy :: Proxy Three)
        it "4D" $
            property $ generalTest p (Proxy :: Proxy Four)
        it "5D" $
            property $ generalTest p (Proxy :: Proxy Five)


generalTest :: (Ord c, Show c)
            => Proxy c -> Proxy n -> [ArbitraryPoint c n] -> Request c n -> Property
generalTest _ _ pointsArb (Request range) =
    let points = map (\(ArbitraryPoint p) -> p) pointsArb
        tree   = build points `restrict` (Proxy :: Proxy Tree)
        ans    = find range $ tree
        nice   = find range $ build points `restrict` (Proxy :: Proxy [])
        extras = whenFail $ putStrLn $ "Built tree is: " ++ show tree
    in  extras $ sort (OrderedPoint <$> ans)
             === sort (OrderedPoint <$> nice)
  where
    restrict :: t (Point c) -> Proxy t -> t (Point c)
    restrict = const

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

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three
type Five  = Succ Four

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
