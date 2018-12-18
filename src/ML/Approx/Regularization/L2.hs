{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
-- | L2 Regularization

module ML.Approx.Regularization.L2 (
    resolve
) where

import Data.Array.IArray (IArray, Array, listArray, elems)
import qualified Data.List as L (transpose, unfoldr)

import Math.Matrix.Core (Matrix, resolveLinearEq, to2dArray, rowLen, toMat, fromMat, add, mul, smul, transpose)

{-# INLINE to1d #-}
to1d :: IArray a e => a (Int, Int) e -> a Int e
to1d ar = let el = elems ar in listArray (0, pred $ length el) el

resolve :: forall e. (Fractional e, Real e) => e -> [(e, e)] -> Maybe [[e]]
resolve _ [] = Nothing
resolve l m = maybe Nothing (\m' -> maybe Nothing (fmap (map (:[]) . elems) . resolveLinearEq m' . to1d . fromMat) $ transpose xs `mul` ys) $ maybe Nothing (add (l `smul` idm)) $ transpose xs `mul` xs
    where
        xs :: Matrix Array Int e
        xs = toMat $ to2dArray $ map (++[1]) $ L.transpose $ L.unfoldr (\i -> if i /= 0 then Just ((^^i) . fst <$> m, pred i) else Nothing) $ length m - 1

        ys :: Matrix Array Int e
        ys = toMat $ to2dArray $ map ((:[]) . snd) m

        idm :: Matrix Array Int e
        idm = let ls = [1..rowLen xs] in toMat (to2dArray [fromIntegral . fromEnum . (x==) <$> ls | x <- ls])
