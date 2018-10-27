{-# OPTIONS_GHC -Wall #-}
module Math.Approx.LEM.ByRecursion (
    linearEqs
) where

import Data.List (unfoldr)
import Data.Maybe (fromJust)
import Data.Tuple.Extra (first)

import Math.Matrix (Matrix, resolveLinearEq)
import Utils (Points)

linearEqs :: (Fractional a, Ord a) => Int -> Points a -> Matrix a
linearEqs n mx = foldr (\x acc -> acc ++ [[x]]) [] $ fromJust $ resolveLinearEq xsums ysums
    where
        xsums = unfoldr (\m -> if m <= n then Just (unfoldr (\x -> if x <= n + m then Just (sum $ map ((^^x) . fst) mx, succ x) else Nothing) m, succ m) else Nothing) 0
        ysums = unfoldr (\m -> if m <= n then Just (sum $ map (uncurry (*) . first (^^m)) mx, succ m) else Nothing) 0
