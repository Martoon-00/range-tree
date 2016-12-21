{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell  #-}

module Test.GeneralSpec
    ( spec
    ) where

import Data.List       (sort)
import Data.Proxy
import Test.Commons    (ArbitraryPoint, ArbitraryPoint (..),
                        OrderedPoint (..), Request (..), ordinal)
import Test.Hspec      (Spec, describe, it)
import Test.QuickCheck (Property, Small, property, whenFail, (===))

import Data.Range.Tree (RangeTree (..), Tree)


spec :: Spec
spec =
    describe "General" $ do
        describe "Double" $
            dimensional $ Proxy @Double
        describe "Int" $
            dimensional $ Proxy @Int

        -- check for processing of equal points
        describe "Small int" $
            dimensional $ Proxy @(Small Int)
        describe "Bool" $
            dimensional $ Proxy @Bool

  where
    dimensional p = do
        it "1D" $
            property $ generalTest @_ @ $(ordinal 1) p
        it "2D" $
            property $ generalTest @_ @ $(ordinal 2) p
        it "3D" $
            property $ generalTest @_ @ $(ordinal 3) p
        it "4D" $
            property $ generalTest @_ @ $(ordinal 4) p
        it "5D" $
            property $ generalTest @_ @ $(ordinal 5) p


generalTest :: (Ord c, Show c)
            => Proxy c -> [ArbitraryPoint d c] -> Request d c -> Property
generalTest _ pointsArb (Request range) =
    let points = map (\(ArbitraryPoint p) -> p) pointsArb
        tree   = build @Tree points
        ans    = find tree range
        nice   = find (build @[] points) range
        extras = whenFail $ putStrLn $ "Built tree is: " ++ show tree
    in  extras $ sort (OrderedPoint <$> ans)
             === sort (OrderedPoint <$> nice)
