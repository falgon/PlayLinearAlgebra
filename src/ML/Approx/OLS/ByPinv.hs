{-# OPTIONS_GHC -Wall #-}
-- | Ordinary Least Squares Regression

module ML.Approx.OLS.ByPinv (
    linearEqs,
) where

import Data.List (unfoldr)

import Utils (Points)
import Math.Matrix (Matrix, transpose, pinv, mmul)

linearEqs :: (Ord a, Fractional a, Real a) => Int -> Points a -> Maybe (Matrix a)
linearEqs n m = map (map fromRational) . (`mmul` map ((:[]) . toRational . snd) m) <$> pinv (coes n)
    where
        coes = map (++[1]) . transpose . unfoldr (\i -> if i /= 0 then Just (toRational . (^^i) . fst <$> m, pred i) else Nothing)
