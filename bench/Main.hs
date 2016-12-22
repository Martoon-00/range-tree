{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TemplateHaskell     #-}

import Control.DeepSeq     (NFData, force)
import Control.Monad       (join, replicateM)
import Control.Monad.Trans (MonadIO (..))
import Criterion.Main      (bench, bgroup, defaultMain, nfIO, Benchmark)
import Data.Monoid         (Sum (..))
import qualified Data.Vector         as V
import Test.Commons        (ArbitraryPoint (..), Numeral, ordinal, Request(..))
import Test.QuickCheck     (Arbitrary (..), generate, vectorOf)

import Data.Range.Tree (Point, RangeTree (..), Tree)

-- | Build tree from given amount randomly generated points
runBuild :: forall d c m . (Ord c, Arbitrary c, Numeral d, MonadIO m)
         => Int -> m (Tree (Point c))
runBuild num = do
    pointsArb <- liftIO . generate $ vectorOf num arbitrary
    let points = extractPoint <$> pointsArb
    return $ build points
  where
    extractPoint :: ArbitraryPoint d c -> Point c
    extractPoint (ArbitraryPoint p) = p

prepareFindBench :: forall d c m . (MonadIO m, Ord c, Arbitrary c, NFData c, Numeral d)
                  => Int -> Int -> m Benchmark
prepareFindBench searches points = do
    -- search for sum of sizes of buckets allows neglect O(|ans|) in evaluation cost
    !tree <- force <$> runBuild @d @c points
    let name = show searches ++ "-lookups-in-" ++ show points ++ "-points"
    return $ bench name . nfIO . replicateM searches $ do
        Request rs <- generate arbitrary :: IO (Request d c)
        return $ findFold tree (Sum . V.length) rs

type D = Double

main :: IO ()
main = do
    findBenchs <- sequence
        [ bgroup "1D" <$> mapM (prepareFindBench @ $(ordinal 1) @D 100 . power 10) [2..4]
        , bgroup "2D" <$> mapM (prepareFindBench @ $(ordinal 2) @D 100 . power 10) [2..3]
        , bgroup "3D" <$> mapM (prepareFindBench @ $(ordinal 3) @D 100 . power 10) [2..2]
        , bgroup "4D" <$> mapM (prepareFindBench @ $(ordinal 4) @D 100 . power 10) [2..2]
        ]
    defaultMain $
        [ bgroup "build"
            [ bgroup "linear"
                [ bgroup "1D" $ benchBuilds @ $(ordinal 1) [100, 200 .. 1000]
                , bgroup "2D" $ benchBuilds @ $(ordinal 2) [100, 200 .. 1000]
                , bgroup "3D" $ benchBuilds @ $(ordinal 3) [100, 200 .. 1000]
                , bgroup "4D" $ benchBuilds @ $(ordinal 4) [100, 200 .. 1000]
                ]
            , bgroup "exponential"
                [ bgroup "1D" $ benchBuilds @ $(ordinal 1) $ power 10 <$> [2..4]
                , bgroup "2D" $ benchBuilds @ $(ordinal 2) $ power 10 <$> [2..4]
                , bgroup "3D" $ benchBuilds @ $(ordinal 3) $ power 10 <$> [2..3]
                , bgroup "4D" $ benchBuilds @ $(ordinal 4) $ power 10 <$> [2..3]
            ]
        ]
        , bgroup "find" findBenchs
        ]
  where
    benchBuilds :: forall d . Numeral d => [Int] -> [Benchmark]
    benchBuilds = fmap $ \n -> bench (show n) . nfIO $ runBuild @d @D n

    power :: Num a => a -> Int -> a
    power _ 0             = 1
    power a n | even n    = join (*) $ power a (n `div` 2)
              | otherwise = power a (n - 1) * a
