{-# OPTIONS_GHC -Wall #-}
module Data.Random.Sample (
    sample
) where

import System.Random (Random)
import Control.Monad (replicateM)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

sample :: (Floating a, Random a) => Int -> Int -> (a -> a) -> IO a -> IO [(a, a)]
sample n i f epsf = zipWith (\x eps -> (fi x, f (fi x+eps))) (take n [i..]) <$> replicateM n epsf
