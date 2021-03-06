{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TemplateHaskell     #-}

import Control.DeepSeq     (NFData, deepseq)
import Control.Monad       (join, replicateM, forM)
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
    tree <- runBuild @d @c points
    let name = show searches ++ "-lookups-in-" ++ show points ++ "-points"
    return . deepseq tree $ bench name . nfIO . replicateM searches $ do
        Request rs <- generate arbitrary :: IO (Request d c)
        return $ findFold tree (Sum . V.length) rs

main :: IO ()
main = do
    findBenchs <- sequence
        [ bgroup "1D" <$> benchFinds @ $(ordinal 1) 1000 (power 2 <$> [6..17])
        , bgroup "2D" <$> benchFinds @ $(ordinal 2) 1000 (power 2 <$> [6..15])
        , bgroup "3D" <$> benchFinds @ $(ordinal 3) 1000 (power 2 <$> [6..12])
        , bgroup "4D" <$> benchFinds @ $(ordinal 4) 1000 (power 2 <$> [6..11])
        ]
    defaultMain $
        [ bgroup "build"
            [ bgroup "1D" $ benchBuilds @ $(ordinal 1) [1000, 2000 .. 10000]
            , bgroup "2D" $ benchBuilds @ $(ordinal 2) [500, 1000 .. 4000]
            , bgroup "3D" $ benchBuilds @ $(ordinal 3) [100, 200 .. 1000]
            , bgroup "4D" $ benchBuilds @ $(ordinal 4) [100, 200 .. 300]
            ]
        , bgroup "find" findBenchs
        ]
  where
    benchBuilds :: forall d . Numeral d => [Int] -> [Benchmark]
    benchBuilds = fmap $ \n -> bench (show n) . nfIO $ runBuild @d @Double n

    benchFinds :: forall d m . (Numeral d, MonadIO m) => Int -> [Int] -> m [Benchmark]
    benchFinds treeSize findsExp = forM findsExp $ prepareFindBench @d @Double treeSize

    power :: Num a => a -> Int -> a
    power _ 0             = 1
    power a n | even n    = join (*) $ power a (n `div` 2)
              | otherwise = power a (n - 1) * a
