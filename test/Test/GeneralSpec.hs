module Test.GeneralSpec
    ( spec
    ) where

import Data.List       (sort)
import Data.Proxy
import Test.Commons    (ArbitraryPoint, ArbitraryPoint (..), Five, Four, One,
                        OrderedPoint (..), Request (..), Three, Two)
import Test.Hspec      (Spec, describe, it)
import Test.QuickCheck (Property, Small, property, whenFail, (===))

import Data.Range.Tree (Point (..), RangeTree (..), Tree)


spec :: Spec
spec =
    describe "General" $ do
        describe "Double" $  -- TODO: go to ghc8
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
