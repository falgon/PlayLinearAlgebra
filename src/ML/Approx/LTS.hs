{-# OPTIONS_GHC -Wall #-}
-- | Least Trimmed Squares Regression
module ML.Approx.LTS (
 --   linearEqs
) where

import Data.List (unfoldr)

import Utils (Points)
import Math.Matrix (Matrix, transpose, pinv, l2Reg, mmul)

{-
linearEqs :: (Ord a, Fractional a, Real a) => Int -> Int -> Points a -> Maybe (Matrix a)
linearEqs l n m = map (map fromRational) . (`mmul` map ((:[]) . toRational . snd) m') <$> pinv (coes n)
    where
        m' = sortBy (\x y -> x^^2 < y^^2

        coes = map (++[1]) . transpose . unfoldr (\i -> if i /= 0 then Just (toRational . (^^i) . fst <$> m', pred i) else Nothing)
        -}
