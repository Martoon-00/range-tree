{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TemplateHaskell     #-}

import Control.Lens        ((<&>))
import Control.Monad.Trans (MonadIO (..))
import Criterion.Main      (bench, bgroup, defaultMain, nfIO)
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
main = defaultMain
    [ bgroup "2D" $
        [10 `power` x :: Int | x <- [2..5]] <&>
            \n -> bench (show n) $ nfIO $
                runBuild @Double n (Proxy @ $(ordinal 2))
    ]
  where
    power :: Int -> Int -> Int
    power a n = foldr (const (* a)) 1 [1..n]
