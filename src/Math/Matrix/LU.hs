{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.Matrix.LU (
    LU (..),
    PLU,
    fromLU,
    rowLength,
    columnLength,
    doolittle,
    doolittleST,
    doolittleST',
    forwards,
    backwards,
    assign,
    resolveLinearEq,
    resolveLinearEq',
    inverse,
    inverse',
    determinant
) where

import Data.Array.IArray (IArray, bounds, listArray, elems, (!))
import Data.Array.ST (Ix, MArray, STArray, newListArray, newArray, readArray, writeArray, thaw, freeze)
import Data.List (foldl1', unfoldr)
import Data.Foldable (foldlM)
import Data.Tuple.Extra (first, second, dupe)
import Data.Maybe (catMaybes)
import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (ST, runST)

{-# INLINE to2dArray #-}
to2dArray :: IArray a e => [[e]] -> a (Int, Int) e
to2dArray [] = listArray ((0, 0), (0, 0)) []
to2dArray xs = listArray ((0, 0), (pred $ length xs, pred $ length $ head xs)) $ concat xs

{-# INLINE swapMArray #-}
swapMArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapMArray ar fi ti | fi == ti = return () | otherwise = do
    to <- readArray ar ti
    readArray ar fi >>= writeArray ar ti
    writeArray ar fi to

newtype LU a i e = LU (a (i, i) e)
newtype Forwarded a i e = Forwarded (a i e) deriving Show

instance (Ix i, Enum i, IArray a e, Show e) => Show (LU a i e) where
    show (LU ar) = "\n" ++ concat (flip map [fst $ fst $ bounds ar..fst $ snd $ bounds ar] $ \i ->
        "{\t" ++ concat (flip map [snd $ fst $ bounds ar..snd $ snd $ bounds ar] $ \j -> show (ar ! (i, j)) ++ "\t") ++ "}\n")

type PLU a i e = (a i i, LU a i e)

{-# INLINE fromLU #-}
fromLU :: (IArray a e, Ix i) => LU a i e -> a (i, i) e
fromLU (LU ar) = ar

{-# INLINE rowLength #-}
rowLength :: (Num i, Ix i, Enum i, IArray a e) => LU a i e -> i
rowLength (LU ar) = succ $ fst (snd $ bounds ar) - fst (fst $ bounds ar)

{-# INLINE columnLength #-}
columnLength :: (Num i, Ix i, Enum i, IArray a e) => LU a i e -> i
columnLength = rowLength

{-# INLINE isSquare #-}
isSquare :: (IArray a e) => a (Int, Int) e -> Bool
isSquare m = let b = bounds m in if fst (fst b) == snd (fst b) && fst (snd b) == snd (snd b) then True else False

{-# INLINE swapAt #-}
swapAt :: Int -> Int -> [a] -> [a]
swapAt ii jj mx
    | ii == jj = mx
    | ii > jj = swapAt jj ii mx
    | otherwise = take ii mx ++ [mx !! jj] ++ take (pred (jj - ii)) (drop (succ ii) mx) ++ [mx !! ii] ++ drop (succ jj) mx

setMaxAbsAt :: (Num a, Ord a) => Int -> [Int] -> [[a]] -> ([Int], [[a]])
setMaxAbsAt i p m
    | length m <= i || length m < 2 || length m /= length p = (p, m)
    | otherwise = let rep = foldl1' (\acc x -> if abs ((m !! x) !! i) > abs ((m !! acc) !! i) then x else acc) [i..pred $ length m] in (swapAt i rep p, swapAt i rep m)
    
forward :: (Fractional e, IArray a Int, IArray a e, Ord e) => [[e]] -> Maybe (PLU a Int e)
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
                | otherwise = (listArray (0, pred $ length p) p, LU $ to2dArray (head u : toUnite 0 l (tail u)))

forwardST :: forall a e. (Ord e, Fractional e, IArray a Int, IArray a e) => a (Int, Int) e -> Maybe (PLU a Int e) 
forwardST m = let len = rowLength $ LU m in runST $ do
        ar <- thaw m :: ST s (STArray s (Int, Int) e) 
        pr <- newListArray (fst $ fst $ bounds m, fst $ snd $ bounds m) [fst $ fst $ bounds m..fst $ snd $ bounds m] :: ST s (STArray s Int Int)
        ($ (fst $ fst $ bounds m)) . fix $ \f i -> if i >= fst (snd $ bounds m) then ((.) Just . (.) (second LU) . (,)) <$> freeze pr <*> freeze ar else do
            r <- foldlM (\acc k -> (\now next -> if abs now < abs next then k else acc) <$> readArray ar (acc, i) <*> readArray ar (k, i)) i [succ i..pred len]
            maxn <- readArray ar (r, i)
            if maxn == 0 then return Nothing else do
                when (r /= i) $ do
                    swapMArray pr r i
                    forM_ [fst (fst $ bounds m).. fst (snd $ bounds m)] $ uncurry (swapMArray ar) . first (i,) . second (r,) . dupe 
                forM_ [succ i..pred len] $ \k -> do
                    writeArray ar (k, i) =<< (/) <$> readArray ar (k, i) <*> readArray ar (i, i)
                    forM_ [succ i..pred len] $ \j -> do
                        kj <- readArray ar (k, j)
                        ij <- readArray ar (i, j)
                        kii <- readArray ar (k, i)
                        writeArray ar (k, j) (kj - ij * kii)
                f $ succ i

-- | doolittle
-- If it is a square and regular matrix, 
-- it returns the exchange information of the row permutation and the LU-decomposed array(It is grouped in one matrix).
-- Otherwise it returns Nothing. Use list for LU factorization.
doolittle ::(Ord e, Fractional e, IArray a Int, IArray a e) => [[e]] -> Maybe (PLU a Int e)
doolittle m
    | length m > 1 && all ((== length m) . length) m = forward m
    | otherwise = Nothing

-- | doolittleST
-- If it is a square and regular matrix, 
-- it returns the exchange information of the row permutation and the LU-decomposed array(It is grouped in one matrix).
-- Otherwise it returns Nothing. Use ST Monad for LU factorization.
doolittleST :: (Ord e, Fractional e, IArray a Int, IArray a e) => a (Int, Int) e -> Maybe (PLU a Int e)
doolittleST m 
    | (==0) . fst $ snd $ bounds m = Nothing
    | uncurry (==) $ snd $ bounds m = forwardST m
    | otherwise = Nothing


-- | doolittleST'
-- It is similar to `doolittleST`. Receive list as input.
doolittleST' :: (Ord e, Fractional e, IArray a Int, IArray a e) => [[e]] -> Maybe (PLU a Int e)
doolittleST' = doolittleST . to2dArray 

-- | forwards
-- First solve Lb = Pv about b
forwards :: forall a e. (Ord e, Fractional e, IArray a Int, IArray a e) => PLU a Int e -> a Int e -> Maybe (Forwarded a Int e)
forwards (p, (LU lu')) v
    | not $ isSquare lu' = Nothing
    | otherwise = Just $ runST $ do
        vr <- thaw v :: ST s (STArray s Int e)
        forM_ [fst $ bounds v..pred $ snd $ bounds v] $ \i -> forM_ [succ i..snd $ bounds v] $ \j -> do
            bi <- readArray vr (p ! i)
            bj <- readArray vr (p ! j) 
            writeArray vr (p ! j) $ bj - bi * lu' ! (j, i)
        Forwarded <$> freeze vr

-- | backwards
-- Finally solve Ux = b about x
backwards :: forall a e. (Ord e, Fractional e, IArray a Int, IArray a e) => PLU a Int e -> Forwarded a Int e -> Maybe (a Int e)
backwards (p, lu@(LU lu')) (Forwarded b)
    | not $ isSquare lu' = Nothing
    | otherwise = let len = rowLength lu in Just $ runST $ do
        br <- thaw b :: ST s (STArray s Int e)
        forM_ [pred len, pred $ pred len..0] $ \i -> do
            writeArray br (p ! i) . (/ lu' ! (i, i)) =<< readArray br (p ! i)
            forM_ [pred i, pred $ pred i..0] $ \j -> do
                bi <- readArray br (p ! i)
                bj <- readArray br (p ! j)
                writeArray br (p ! j) $ bj - bi * lu' ! (j, i)
        s <- newArray (bounds b) 0 :: ST s (STArray s Int e)
        forM_ [fst $ bounds b..snd $ bounds b] $ \i -> writeArray s i =<< readArray br (p ! i)
        freeze s

-- | assign
-- Forward and backward to LUx = Pv
assign :: forall a e. (Ord e, Fractional e, IArray a Int, IArray a e) => PLU a Int e -> a Int e -> Maybe (a Int e)
assign plu = maybe Nothing (backwards plu) . forwards plu

-- | resolveLinearEq
-- Solve the n simultaneous equations by PLU decomposition. `mxr` and `v` must be the same size.
resolveLinearEq :: (Ord e, Fractional e, IArray a Int, IArray a e) => a (Int, Int) e -> a Int e -> Maybe (a Int e)
resolveLinearEq mxr v = maybe Nothing (`assign` v) $ doolittleST mxr

-- | resolveLinearEq'
-- It is similar to `resolveLinearEq`. Receive list as input.
resolveLinearEq' :: (Ord e, Fractional e, IArray a Int, IArray a e) => [[e]] -> [e] -> Maybe (a Int e)
resolveLinearEq' mxr v = resolveLinearEq (to2dArray mxr) $ listArray (0, pred $ length v) v

-- | inverse
-- Compute inverse value by LU decomposition
inverse :: (Ord e, Fractional e, IArray a Int, IArray a e) => a (Int, Int) e -> Maybe (a (Int, Int) e)
inverse mxr 
    | not $ isSquare mxr = Nothing
    | otherwise = let len = succ $ fst (snd $ bounds mxr) - fst (fst $ bounds mxr); cal = calcs len in
        if length cal /= len then Nothing else 
            Just $ listArray ((fst $ fst $ bounds mxr, fst $ fst $ bounds mxr), (pred len, pred len)) $ concat $ unfoldr (\x -> if x < len then Just (map (! x) cal, succ x) else Nothing) 0
    where
        unitVecs i = replicate i 0 ++ [1] ++ repeat 0
        calcs len = flip (maybe []) (doolittleST mxr) $ \plu -> catMaybes $
            unfoldr (\x -> if x < len then Just (plu `assign` (listArray (fst $ fst $ bounds mxr, pred len) $ take len (unitVecs x)), succ x) else Nothing) 0

-- | inverse'
-- It is similar to `inverse`. Receive list as input.
inverse' :: (Ord e, Fractional e, IArray a Int, IArray a e) => [[e]] -> Maybe (a (Int, Int) e)
inverse' = inverse . to2dArray


-- | determinant
-- Compute determinant by LU decomposition.
-- |A| = |LU| = |L||U| = product u_ii because L is a lower triangular matrix and U is upper triangular matrix.
determinant :: (Ord e, Fractional e, IArray a Int, IArray a e) => a (Int, Int) e -> e
determinant mxr 
    | not $ isSquare mxr = 0
    | otherwise = flip (maybe 0) (doolittleST mxr) $ 
        \(p, lu@(LU lu')) -> (if sign p then -1 else 1) * product (unfoldr (\x -> if x < rowLength lu then Just (lu' ! (x, x), succ x) else Nothing) 0)
    where
        {-# INLINE dropAt #-}
        dropAt i xs = take i xs ++ drop (succ i) xs
        sign pp = even $ (sum :: [Integer] -> Integer) $ flip unfoldr (elems pp, 0) $
            \(pps, i) -> if length pps == 0 then Nothing else let (p:ps) = pps in 
                if p == i then Just (0, (ps, succ i)) else Just (1, (dropAt (pred p - i) ps, succ i))
