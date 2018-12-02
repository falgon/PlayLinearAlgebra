{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Math.Matrix.LU (
    doolittleST
) where

import Data.Array.Unboxed (UArray)
import Data.Array.ST (Ix, MArray, STUArray, newListArray, readArray, writeArray, freeze)
import Data.Bool (bool)
import Data.Foldable (foldrM)
import Data.Tuple.Extra (first, second, dupe)
import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (ST, runST)

{-# INLINE from2dList #-}
from2dList :: MArray a e m => [[e]] -> m (a (Int, Int) e)
from2dList [] = newListArray ((0, 0), (0, 0)) []
from2dList xs = newListArray ((0, 0), (pred $ length xs, pred $ length $ head xs)) $ concat xs

{-# INLINE swapMArray #-}
swapMArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapMArray ar fi ti = do
    to <- readArray ar ti
    readArray ar fi >>= writeArray ar ti
    writeArray ar fi to

{-
forwardST :: [[Double]] -> Maybe ([Int], [[Double]]) 
forwardST m = let len = length $ head m in
    Just $ first fst $ second (unfoldr (\x -> if length x /= 0 then Just (take len x, drop len x) else Nothing) . snd) $ dupe $ runST $ do
        ar <- from2dList m :: ST s (STUArray s (Int, Int) Double)
        pr <- newListArray (0, pred len) [0..pred len] :: ST s (STUArray s Int Int)
        forM_ [0..pred len] $ \i -> do
            now <- readArray ar (i, i)
            r <- flip (flip foldrM i) [succ i..pred len] $ \k acc -> do
                next <- readArray ar (k, i)
                return $ if abs next > abs now then k else acc
            --if readArray ar (r, i) == 0
            when (r /= i) $ do
                swapMArray pr r i
                forM_ [i..pred len] $ \k -> swapMArray ar (i, k) (r, k)
            
            forM_ [succ i..pred len] $ \k -> do
                ki <- readArray ar (k, i)
                ii <- readArray ar (i, i)
                writeArray ar (k, i) (ki / ii)
                forM_ [succ i, pred len] $ \j -> do
                    kj <- readArray ar (k, j)
                    ij <- readArray ar (i, j)
                    kii <- readArray ar (k, i)
                    writeArray ar (k, j) (kj - ij * kii)
        a <- getElems ar
        p <- getElems pr
        return (p, a)
-}
   -- maybe Nothing (Just . first fst . second (unfoldr (\x -> if length x /= 0 then Just (take len x, drop len x) else Nothing) . snd) . dupe) $ runST $ do

forwardST :: [[Double]] -> Maybe (UArray Int Int, UArray (Int, Int) Double)
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

-- doolittleST :: [[Double]] -> Maybe ([Int], [[Double]])
-- doolittleST :: [[Double]] -> Maybe (IArray s (Int, Double)
doolittleST :: [[Double]] -> Maybe (UArray Int Int, UArray (Int, Int) Double)
doolittleST [] = Nothing
doolittleST m 
    | (length m > 1) && all ((>= length m) . length) m = forwardST m
    | otherwise = Nothing



