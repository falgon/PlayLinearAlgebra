{-# OPTIONS_GHC -Wall #-}
module ML.Approx.OLS.ByLU (
    resolve
) where

import Data.List (unfoldr)
import Data.Tuple.Extra (first)
import Data.Array.IArray (Array, elems)

import Math.Matrix.Core (resolveLinearEq')

{-# INLINE xsums #-}
xsums :: (Real e, Fractional e) => Int -> [(e, e)] -> [[Rational]]
xsums n mx = map toRational <$> unfoldr (\m -> if m <= n then Just (unfoldr (\x -> if x <= n + m then Just (sum $ map ((^^x) . fst) mx, succ x) else Nothing) m, succ m) else Nothing) 0

{-# INLINE ysums #-}
ysums :: (Real e, Fractional e) => Int -> [(e, e)] -> [Rational]
ysums n mx = toRational <$> unfoldr (\m -> if m <= n then Just (sum $ map (uncurry (*) . first (^^m)) mx, succ m) else Nothing) 0

resolve :: (Ord e, Fractional e, Real e) => Int -> [(e, e)] -> Maybe [[e]]
resolve _ [] = Nothing
resolve n mx = fmap (foldr (\x acc -> acc ++ [[fromRational x]]) [] . elems) (resolveLinearEq' (xsums n mx) (ysums n mx) :: Maybe (Array Int Rational))

