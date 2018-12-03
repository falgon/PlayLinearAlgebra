{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Math.Matrix.LU (
    PLU (..),
    fromPLU,
    doolittleST,
    doolittleST'
) where

import Data.Array.IArray (IArray, bounds, listArray, (!))
import Data.Array.ST (Ix, MArray, STUArray, newListArray, readArray, writeArray, thaw, freeze)
import Data.Bool (bool)
import Data.Foldable (foldrM)
import Data.Tuple.Extra (first, second, dupe)
import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (ST, runST)

{-
{-# INLINE from2dList #-}
from2dList :: MArray a e m => [[e]] -> m (a (Int, Int) e)
from2dList [] = newListArray ((0, 0), (0, 0)) []
from2dList xs = newListArray ((0, 0), (pred $ length xs, pred $ length $ head xs)) $ concat xs
-}

{-# INLINE to2dArray #-}
to2dArray :: IArray a e => [[e]] -> a (Int, Int) e
to2dArray [] = listArray ((0, 0), (0, 0)) []
to2dArray xs = listArray ((0, 0), (pred $ length xs, pred $ length $ head xs)) $ concat xs

{-# INLINE swapMArray #-}
swapMArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapMArray ar fi ti = do
    to <- readArray ar ti
    readArray ar fi >>= writeArray ar ti
    writeArray ar fi to

{-
forwardST :: (IArray a Int, IArray a Double) => [[Double]] -> Maybe (a Int Int, a (Int, Int) Double) 
forwardST m = let len = length $ head m in runST $ do
        ar <- from2dList m :: ST s (STUArray s (Int, Int) Double)
        pr <- newListArray (0, pred len) [0..pred len] :: ST s (STUArray s Int Int)
        ($ 0) . fix $ \f i -> if i >= len then (.) Just . (,) <$> freeze pr <*> freeze ar else do
            now <- readArray ar (i, i)
            r <- foldrM (\k acc -> bool acc k . (flip (>) (abs now)) . abs <$> readArray ar (k, i)) i [succ i..pred len]
            maxn <- readArray ar (r, i)
            if maxn == 0 then return Nothing else do
                when (r /= i) $ do
                    swapMArray pr r i
                    forM_ [i..pred len] $ uncurry (swapMArray ar) . first (i,) . second (r,) . dupe 
                forM_ [succ i..pred len] $ \k -> do
                    writeArray ar (k, i) =<< (/) <$> readArray ar (k, i) <*> readArray ar (i, i)
                    forM_ [succ i..pred len] $ \j -> do
                        kj <- readArray ar (k, j)
                        ij <- readArray ar (i, j)
                        kii <- readArray ar (k, i)
                        writeArray ar (k, j) (kj - ij * kii)
                f $ succ i
-}

data PLU a i e = PLU (a (i, i) e)

instance (Ix i, Enum i, IArray a e, Show e) => Show (PLU a i e) where
    show (PLU ar) = "\n" ++ concat (flip map [fst $ fst $ bounds ar..fst $ snd $ bounds ar] $ \i ->
        "{\t" ++ (concat $ flip map [snd $ fst $ bounds ar..snd $ snd $ bounds ar] $ \j -> show (ar ! (i, j)) ++ "\t") ++ "}\n")

fromPLU :: PLU a i e -> a (i, i) e
fromPLU (PLU ar) = ar

forwardST :: (IArray a Int, IArray a Double) => a (Int, Int) Double -> Maybe (a Int Int, PLU a Int Double) 
forwardST m = let len = succ $ snd $ snd $ bounds m in runST $ do
        ar <- thaw m :: ST s (STUArray s (Int, Int) Double)
        pr <- newListArray (0, pred len) [0..pred len] :: ST s (STUArray s Int Int)
        ($ 0) . fix $ \f i -> if i >= len then ((.) Just . (.) (second PLU) . (,))  <$> freeze pr <*> freeze ar else do
            now <- readArray ar (i, i)
            r <- foldrM (\k acc -> bool acc k . (flip (>) (abs now)) . abs <$> readArray ar (k, i)) i [succ i..pred len]
            maxn <- readArray ar (r, i)
            if maxn == 0 then return Nothing else do
                when (r /= i) $ do
                    swapMArray pr r i
                    forM_ [i..pred len] $ uncurry (swapMArray ar) . first (i,) . second (r,) . dupe 
                forM_ [succ i..pred len] $ \k -> do
                    writeArray ar (k, i) =<< (/) <$> readArray ar (k, i) <*> readArray ar (i, i)
                    forM_ [succ i..pred len] $ \j -> do
                        kj <- readArray ar (k, j)
                        ij <- readArray ar (i, j)
                        kii <- readArray ar (k, i)
                        writeArray ar (k, j) (kj - ij * kii)
                f $ succ i

doolittleST :: (IArray a Int, IArray a Double) => a (Int, Int) Double -> Maybe (a Int Int, PLU a Int Double)
doolittleST m 
    | (==0) . fst $ snd $ bounds m = Nothing
    | uncurry (==) $ snd $ bounds m = forwardST m
    | otherwise = Nothing


doolittleST' :: (IArray a Int, IArray a Double) => [[Double]] -> Maybe (a Int Int, PLU a Int Double)
doolittleST' = doolittleST . to2dArray 

-- | doolittleST
-- If it is a square and regular matrix, 
-- it returns the exchange information of the row permutation and the LU-decomposed array(It is grouped in one matrix).
-- Otherwise it returns Nothing.
{-
doolittleST :: (IArray a Int, IArray a Double) => [[Double]] -> Maybe (a Int Int, a (Int, Int) Double) 
doolittleST [] = Nothing
doolittleST m 
    | (length m > 1) && all ((>= length m) . length) m = forwardST m
    | otherwise = Nothing
-}
