{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Math.Matrix.LU (
    PLU (..),
    fromPLU,
    doolittle,
    doolittleST,
    doolittleST'
) where

import Data.Array.IArray (IArray, bounds, listArray, (!))
import Data.Array.ST (Ix, MArray, STUArray, newListArray, readArray, writeArray, thaw, freeze)
import Data.Bool (bool)
import Data.List (foldl1')
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

newtype PLU a i e = PLU (a (i, i) e)

instance (Ix i, Enum i, IArray a e, Show e) => Show (PLU a i e) where
    show (PLU ar) = "\n" ++ concat (flip map [fst $ fst $ bounds ar..fst $ snd $ bounds ar] $ \i ->
        "{\t" ++ concat (flip map [snd $ fst $ bounds ar..snd $ snd $ bounds ar] $ \j -> show (ar ! (i, j)) ++ "\t") ++ "}\n")

{-# INLINE fromPLU #-}
fromPLU :: PLU a i e -> a (i, i) e
fromPLU (PLU ar) = ar

{-# INLINE swapAt #-}
swapAt :: Int -> Int -> [a] -> [a]
swapAt ii jj mx
    | ii == jj = mx
    | ii > jj = swapAt jj ii mx
    | otherwise = take ii mx ++ [mx !! jj] ++ take (pred (jj - ii)) (drop (succ ii) mx) ++ [mx !! ii] ++ drop (succ jj) mx

setMaxAbsAt :: (Num a, Ord a) => Int -> [Int] -> [[a]] -> ([Int], [[a]])
setMaxAbsAt i p m
    | length m <= i || length m < 2 || length m /= length p = (p, m)
    | otherwise = let rep = foldl1' (\x acc -> if abs ((m !! x) !! i) > abs ((m !! acc) !! i) then x else acc) [i..pred $ length m] in (swapAt i rep p, swapAt i rep m)

forward :: (Fractional e, IArray a1 Int, IArray a2 e, Ord e) => [[e]] -> Maybe (a1 Int Int, PLU a2 Int e)
forward m
    | length m < 2 || any ((< 2) . length) m = Nothing
    | all ((==) (length m) . length) m = Just $ forward' 0 [0..pred $ length m] [] m
    | otherwise = Nothing
        where
            elim i mx = let use = (mx !! i) !! i; l = map ((/use) . flip (!!) i) $ drop (succ i) mx in 
                (l, take (succ i) mx ++ zipWith (\row ll -> zipWith (+) row $ map (negate . (*ll)) (mx !! i)) (drop (succ i) mx) l)
            
            toUnite _ [] u = u
            toUnite i (l:ls) u = let (uu:uus) = zipWith (\al urow -> take i urow ++ [al] ++ drop (succ i) urow) l u in uu : toUnite (succ i) ls uus

            forward' i p l u
                | i < pred (length u) = let (pp, mx) = setMaxAbsAt i p u in uncurry (forward' (succ i) pp) $ first ((l ++) . (:[])) $ elim i mx
                | otherwise = (listArray (0, pred $ length p) p, PLU $ to2dArray (head u : toUnite 0 l (tail u)))

-- | doolittleST
-- If it is a square and regular matrix, 
-- it returns the exchange information of the row permutation and the LU-decomposed array(It is grouped in one matrix).
-- Otherwise it returns Nothing. Use List for LU factorization.
doolittle ::(Fractional e, IArray a1 Int, IArray a2 e, Ord e) => [[e]] -> Maybe (a1 Int Int, PLU a2 Int e)
doolittle m
    | length m > 1 && all ((==(length m)) . length) m = forward m
    | otherwise = Nothing

forwardST :: (IArray a Int, IArray a Double) => a (Int, Int) Double -> Maybe (a Int Int, PLU a Int Double) 
forwardST m = let len = succ $ snd $ snd $ bounds m in runST $ do
        ar <- thaw m :: ST s (STUArray s (Int, Int) Double)
        pr <- newListArray (0, pred len) [0..pred len] :: ST s (STUArray s Int Int)
        ($ 0) . fix $ \f i -> if i >= len then ((.) Just . (.) (second PLU) . (,))  <$> freeze pr <*> freeze ar else do
            now <- readArray ar (i, i)
            r <- foldrM (\k acc -> bool acc k . (> abs now) . abs <$> readArray ar (k, i)) i [succ i..pred len]
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

-- | doolittleST
-- If it is a square and regular matrix, 
-- it returns the exchange information of the row permutation and the LU-decomposed array(It is grouped in one matrix).
-- Otherwise it returns Nothing. Use ST Monad for LU factorization.
doolittleST :: (IArray a Int, IArray a Double) => a (Int, Int) Double -> Maybe (a Int Int, PLU a Int Double)
doolittleST m 
    | (==0) . fst $ snd $ bounds m = Nothing
    | uncurry (==) $ snd $ bounds m = forwardST m
    | otherwise = Nothing


-- | doolittleST'
-- It is similar to doolittleST. Receive list as input.
doolittleST' :: (IArray a Int, IArray a Double) => [[Double]] -> Maybe (a Int Int, PLU a Int Double)
doolittleST' = doolittleST . to2dArray 

