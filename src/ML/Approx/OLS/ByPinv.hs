-- | Ordinary Least Squares Regression
{-# OPTIONS_GHC -Wall #-}

module ML.Approx.OLS.ByPinv (
    resolve
) where

import qualified Data.List as L

import Math.Matrix.Core (Matrix, toMat, toList, mul, to2dArray, pseudoInverse')
import Data.Array.IArray (Array)

resolve :: (Ord a, Fractional a, Real a) => Int -> [(a, a)] -> Maybe [[a]]
resolve n m = maybe Nothing (fmap (map (map fromRational) . toList) . (`mul` sn)) $ pseudoInverse' (coes n)
    where
        coes = map (++[1]) . L.transpose . L.unfoldr (\i -> if i /= 0 then Just (toRational . (^^i) . fst <$> m, pred i) else Nothing)
        sn :: Matrix Array Int Rational
        sn = toMat $ to2dArray $ map ((:[]) . toRational . snd) m
