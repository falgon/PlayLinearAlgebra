{-# OPTIONS_GHC -Wall #-}
module ML.Approx.OLS.ByRecursion (
    linearEqs
) where

import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (first)

import Math.Matrix (Matrix, resolveLinearEq)
import Utils (Points)

linearEqs :: (Ord a, Fractional a, Real a) => Int -> Points a -> Maybe (Matrix a)
linearEqs _ [] = Nothing
linearEqs n mx = Just $ foldr (\x acc -> acc ++ [[fromRational x]]) [] $ fromJust $ resolveLinearEq (map (map toRational) xsums) $ map toRational ysums
    where
        xsums = unfoldr (\m -> if m <= n then Just (unfoldr (\x -> if x <= n + m then Just (sum $ map ((^^x) . fst) mx, succ x) else Nothing) m, succ m) else Nothing) 0
        ysums = unfoldr (\m -> if m <= n then Just (sum $ map (uncurry (*) . first (^^m)) mx, succ m) else Nothing) 0

