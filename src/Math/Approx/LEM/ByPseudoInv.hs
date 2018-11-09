{-# OPTIONS_GHC -Wall #-}
module Math.Approx.LEM.ByPseudoInv (
    linearEqs,
    linearEqsRegular
) where

import Data.List (unfoldr)

import Utils (Points)
import Math.Matrix (Matrix, transpose, leastSquarePinv, leastSquarePinvRegular, mmul)

linearEqs :: (Ord a, Fractional a, Real a) => Int -> Points a -> Maybe (Matrix a)
linearEqs n m = map (map fromRational) . (`mmul` map ((:[]) . toRational . snd) m) <$> leastSquarePinv (coes n)
    where
        coes = map (++[1]) . transpose . unfoldr (\i -> if i /= 0 then Just (toRational . (^^i) . fst <$> m, pred i) else Nothing)

linearEqsRegular :: (Ord a, Fractional a, Real a) => a -> Points a -> Maybe (Matrix a)
linearEqsRegular l m = map (map fromRational) . (`mmul` map ((:[]). toRational . snd) m) <$> leastSquarePinvRegular l m
