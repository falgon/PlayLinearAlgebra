{-# OPTIONS_GHC -Wall #-}
module Math.Matrix (
    Matrix,
    gaussianElim,
    resolveLinearEq,
    determinant,
    mmul,
    inv,
    pinv,
    transpose
) where

import Data.Bool (bool)
import Data.Maybe (maybe, fromJust)
import Data.List (transpose)
import Data.Tuple.Extra (first, second, dupe)

type Matrix a = [[a]]

swapElems :: Int -> Int -> [a] -> [a]
swapElems i j xs | i > j = swapElems j i xs | otherwise = take i xs ++ [xs !! j] ++ take (j - i - 1) (drop (i + 1) xs) ++ [xs !! i] ++ drop (j + 1) xs

setMaxAbs :: (Num a, Ord a) => Int -> [[a]] -> [[a]]
setMaxAbs i = uncurry (++) . first (take i) . second (setMaxAbs' i . drop i) . dupe
    where
        setMaxAbs' ii row
            | succ ii < length row = setMaxAbs' (succ ii) $ if abs ((row !! ii) !! ii) < abs ((row !! succ ii) !! ii) then swapElems ii (succ ii) row else row
            | otherwise = row

forward :: (Fractional a, Ord a) => [[a]] -> [[a]]
forward = forward' 0
    where
        forward' n xs | length xs - 1 > n = forward' (succ n) $ toZero n $ setMaxAbs n xs | otherwise = xs
        toZero i xs = take (i + 1) xs ++ [let c = (r !! i) / (ur !! i) in zipWith ((+) . negate . (*c)) ur r | r <- drop (i + 1) xs]
            where
                ur = xs !! i

backward :: (Fractional a, Ord a) => [[a]] -> [[a]]
backward = uncurry backward' . first ((+(-1)) . length) . dupe
    where
        backward' n xs | n >= 0 = backward' (pred n) $ toZero n xs | otherwise = xs
        toZero i xs = [let ur = xs !! i; c = (r !! i) / (ur !! i) in zipWith ((+) . negate . (*c)) ur r | r <- take i xs] ++ drop i xs

cofactorExpand :: Num a => Int -> [[a]] -> a
cofactorExpand _ [[x]] = x
cofactorExpand _ [[x1, y1], [x2, y2]] = x1 * y2 - y1 * x2
cofactorExpand i m = expand i m
    where
        expand ii mx = sum $ (\(e, j) -> (-1)^(ii+j) * e * cofactorExpand (succ ii) (takeCofactor j mx)) <$> zip (head mx) (take (length mx) (iterate (+1) 0))
        takeCofactor j mx = (uncurry (++) . first (take j) . second (drop (j + 1)) . dupe) <$> drop 1 mx

mmul :: (Num a) => [[a]] -> [[a]] -> [[a]]
mmul lm rm = [[sum $ zipWith (*) l r | r <- transpose rm] | l <- lm]

gaussianElim :: (Fractional a, Ord a) => [[a]] -> Maybe [[a]]
gaussianElim m | (length m > 1) && all ((>= length m) . length) m = Just $ backward $ forward m | otherwise = Nothing

resolveLinearEq :: (Fractional a, Ord a) => [[a]] -> [a] -> Maybe [a]
resolveLinearEq = (.) (fmap (map (uncurry (/) . first last . second (sum . init) . dupe))) . (.) gaussianElim . zipWith (flip (.) (:[]) . (++))

determinant :: Num a => [[a]] -> Maybe a
determinant m | all ((== length m) . length) m = Just $ cofactorExpand 1 m | otherwise = Nothing

inv :: (Fractional a, Ord a) => [[a]] -> Maybe [[a]]
inv m = ($ determinant m) $ maybe Nothing $ \d -> if d == 0 then Nothing else
    flip fmap (gaussianElim $ zipWith (++) m $ idm $ length m) $ \gs -> [let half = length (head gs) `div` 2 in (/(sum $ take half g)) <$> drop half g | g <- gs]
    where
        idm n = let xs = [1 .. n] in [fromIntegral . fromEnum . (x==) <$> xs | x <- xs]

pinv :: (Fractional a, Ord a) => [[a]] -> [[a]]
pinv [] = []
pinv m = ($ determinant m) $ maybe pinv' $ bool pinv' (fromJust $ inv m) . (/=0)
    where
        pinv' = if length (head m) < length m then fromJust (inv (transpose m `mmul` m)) `mmul` transpose m else transpose m `mmul` fromJust (inv (m `mmul` transpose m))
