{-# OPTIONS_GHC -Wall #-}
module ML.Approx.OLS.ByLU (
    xsums,
    ysums,
    resolve
) where

import Data.List (unfoldr)
import Data.Tuple.Extra (first)
import Data.Array.IArray (Array, elems)

import Math.Matrix.Core (resolveLinearEq')
import Utils (Points)

{-# INLINE xsums #-}
xsums :: (Real e, Fractional e) => Int -> Points e -> [[Rational]]
xsums n mx = map toRational <$> unfoldr (\m -> if m <= n then Just (unfoldr (\x -> if x <= n + m then Just (sum $ map ((^^x) . fst) mx, succ x) else Nothing) m, succ m) else Nothing) 0

{-# INLINE ysums #-}
ysums :: (Real e, Fractional e) => Int -> Points e -> [Rational]
ysums n mx = toRational <$> unfoldr (\m -> if m <= n then Just (sum $ map (uncurry (*) . first (^^m)) mx, succ m) else Nothing) 0

resolve :: (Ord e, Fractional e, Real e) => Int -> Points e -> Maybe [[e]]
resolve _ [] = Nothing
resolve n mx = fmap (foldr (\x acc -> acc ++ [[fromRational x]]) [] . elems) $
    (resolveLinearEq' (xsums n mx) (ysums n mx) :: Maybe (Array Int Rational))

