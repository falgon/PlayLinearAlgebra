{-# OPTIONS_GHC -Wall #-}
-- | L2 Regularization

module ML.Approx.Regularization.L2 (
    linearEqs
) where

import Utils (Points)
import Math.Matrix (Matrix, l2Reg, mmul)

linearEqs :: (Ord a, Fractional a, Real a) => a -> Points a -> Maybe (Matrix a)
linearEqs l m = map (map fromRational) . (`mmul` map ((:[]). toRational . snd) m) <$> l2Reg l m
