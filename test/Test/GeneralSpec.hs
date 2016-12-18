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
        describe "Double" $  -- TODO: go to ghc8
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
            property $ generalTest p $ Proxy @ $(ordinal 1)
        it "2D" $
            property $ generalTest p $ Proxy @ $(ordinal 2)
        it "3D" $
            property $ generalTest p $ Proxy @ $(ordinal 3)
        it "4D" $
            property $ generalTest p $ Proxy @ $(ordinal 4)
        it "5D" $
            property $ generalTest p $ Proxy @ $(ordinal 5)


generalTest :: (Ord c, Show c)
            => Proxy c -> Proxy n -> [ArbitraryPoint c n] -> Request c n -> Property
generalTest _ _ pointsArb (Request range) =
    let points = map (\(ArbitraryPoint p) -> p) pointsArb
        tree   = build @Tree points
        ans    = find range $ tree
        nice   = find range $ build @[] points
        extras = whenFail $ putStrLn $ "Built tree is: " ++ show tree
    in  extras $ sort (OrderedPoint <$> ans)
             === sort (OrderedPoint <$> nice)
