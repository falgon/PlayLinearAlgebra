{-# OPTIONS_GHC -Wall #-}
module Math.Approx.LEM.ByPseudoInv (
    linearEqs
) where

import Data.List (unfoldr)

import Utils (Points)
import Math.Matrix (Matrix, transpose, pinv, mmul)

linearEqs :: (Fractional a, Ord a) => Int -> Points a -> Matrix a
linearEqs n m | 1 > n = [] | otherwise = pinv (coes n) `mmul` map ((:[]) . snd) m
    where
        coes = map (++[1]) . transpose . unfoldr (\i -> if i /= 0 then Just ((^^i) . fst <$> m, pred i) else Nothing)
