{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TemplateHaskell     #-}

import Control.Lens        ((<&>))
import Control.Monad       (join)
import Control.Monad.Trans (MonadIO (..))
import Criterion.Main      (bench, bgroup, defaultMain, nfIO, Benchmark)
import Data.Proxy          (Proxy (..))
import Test.Commons        (ArbitraryPoint (..), Numeral, ordinal)
import Test.QuickCheck     (Arbitrary (..), generate, vectorOf)

import Data.Range.Tree (Point, RangeTree (..), Tree)

runBuild :: (Ord c, Arbitrary c, Numeral n, MonadIO m)
         => Int -> Proxy n -> m (Tree (Point c))
runBuild num d = do
    pointsArb <- liftIO $ generate $ vectorOf num arbitrary
    let points = extractPoint d <$> pointsArb
    return $ build points
  where
    extractPoint :: Proxy n -> ArbitraryPoint c n -> Point c
    extractPoint _ (ArbitraryPoint p) = p

main :: IO ()
main = defaultMain $
    [ bgroup "1D" $ dimensional @ $(ordinal 1)
    , bgroup "2D" $ dimensional @ $(ordinal 2)
    , bgroup "3D" $ dimensional @ $(ordinal 3)
    , bgroup "4D" $ dimensional @ $(ordinal 4)
    , bgroup "5D" $ dimensional @ $(ordinal 5)
    ]
  where
    dimensional :: forall d . Numeral d => [Benchmark]
    dimensional =
        [10 `power` x | x :: Int <- [2..5]]
            <&> \n -> bench (show n) . nfIO $
                runBuild @Double n (Proxy @d)

    power _ 0             = 1
    power a n | even n    = join (*) $ power a (n `div` 2)
              | otherwise = power a (n - 1) * a
