{-# OPTIONS_GHC -Wall #-}
module Math.Approx.LEM.ByPseudoInv (
    linearEqs,
    linearEqsRegular
) where

import Data.List (unfoldr)
import Data.Maybe (fromJust)

import Utils (Points)
import Math.Matrix (Matrix, transpose, leastSquarePinv, leastSquarePinvRegular, mmul)

linearEqs :: (Ord a, Fractional a, Real a) => Int -> Points a -> Matrix a
linearEqs n m | 1 > n = [] | otherwise = fromJust (leastSquarePinv (coes n)) `mmul` map ((:[]) . snd) m
    where
        coes = map (++[1]) . transpose . unfoldr (\i -> if i /= 0 then Just ((^^i) . fst <$> m, pred i) else Nothing)

linearEqsRegular :: (Ord a, Fractional a, Real a) => a -> Points a -> Matrix a
linearEqsRegular n m | 1 > n = [] | otherwise = fromJust (leastSquarePinvRegular n (coes (length m))) `mmul` map ((:[]) . snd) m
    where
        coes = map (++[1]) . transpose . unfoldr (\i -> if i /= 0 then Just ((^^i) . fst <$> m, pred i) else Nothing)
