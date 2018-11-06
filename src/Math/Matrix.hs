{-# OPTIONS_GHC -Wall #-}
module Math.Matrix (
    Matrix,
    resolveLinearEq,
    determinant,
    madd,
    mmul,
    inv,
    leastSquarePinv,
    leastSquarePinvRegular,
    transpose
) where

import Data.Maybe (fromJust)
import Data.List (transpose, unfoldr)
import Data.Tuple.Extra (first, second, dupe)

type Matrix a = [[a]]

swapElems :: Int -> Int -> [a] -> [a]
swapElems i j xs 
    | i > j = swapElems j i xs 
    | otherwise = take i xs ++ [xs !! j] ++ take (j - i - 1) (drop (i + 1) xs) ++ [xs !! i] ++ drop (j + 1) xs

setMaxAbs :: (Num a, Ord a) => Int -> [[a]] -> [[a]]
setMaxAbs i = uncurry (++) . first (take i) . second (setMaxAbs' i . drop i) . dupe
    where
        setMaxAbs' ii row
            | succ ii < length row = setMaxAbs' (succ ii) $ if abs ((row !! ii) !! ii) < abs ((row !! succ ii) !! ii) then swapElems ii (succ ii) row else row
            | otherwise = row

forward :: (Ord a, Fractional a, Real a) => [[a]] -> Maybe [[Rational]]
forward = forward' 0 . map (map toRational)
    where
        forward' n xs
            | length xs - 1 > n = maybe Nothing (forward' (succ n)) $ toZero n (setMaxAbs n xs)
            | otherwise = Just xs
        toZero i xs 
            | ((xs !! i) !! i) == 0 = Nothing
            | otherwise = Just $ take (succ i) xs ++ [let ur = xs !! i; c = (r !! i) / (ur !! i) in zipWith ((+) . negate . (*c)) ur r | r <- drop (i + 1) xs]

backward :: Fractional a => [[Rational]] -> Maybe [[a]]
backward = uncurry backward' . first ((+(-1)) . length) . dupe
    where
        backward' n xs
            | n >= 0 = maybe Nothing (backward' (pred n)) $ toZero n xs
            | otherwise = Just $ map (map fromRational) xs
        toZero i xs 
            | ((xs !! i) !! i) == 0 = Nothing
            | otherwise = Just $ [let ur = xs !! i; c = (r !! i) / (ur !! i) in zipWith ((+) . negate . (*c)) ur r | r <- take i xs] ++ drop i xs

gaussianElim :: (Ord a, Fractional a, Real a) => [[a]] -> Maybe [[a]]
gaussianElim m
    | (length m > 1) && all ((>= length m) . length) m = maybe Nothing backward $ forward m
    | otherwise = Nothing

cofactorExpand :: Num a => Int -> [[a]] -> a
cofactorExpand _ [[x]] = x
cofactorExpand _ [[x1, y1], [x2, y2]] = x1 * y2 - y1 * x2
cofactorExpand i m = expand i m
    where
        expand ii mx = sum $ (\(e, j) -> (-1)^(ii+j) * e * cofactorExpand (succ ii) (takeCofactor j mx)) <$> zip (head mx) (take (length mx) (iterate (+1) 0))
        takeCofactor j mx = (uncurry (++) . first (take j) . second (drop (j + 1)) . dupe) <$> drop 1 mx

madd :: Num a => [[a]] -> [[a]] -> [[a]]
madd = zipWith (zipWith (+))

mmul :: Num a => [[a]] -> [[a]] -> [[a]]
mmul lm rm = [[sum $ zipWith (*) l r | r <- transpose rm] | l <- lm]

resolveLinearEq :: (Ord a, Fractional a, Real a) => [[a]] -> [a] -> Maybe [a]
resolveLinearEq = (.) (fmap (map (uncurry (/) . first last . second (sum . init) . dupe))) . (.) gaussianElim . zipWith (flip (.) (:[]) . (++))

determinant :: Num a => [[a]] -> Maybe a
determinant m | all ((== length m) . length) m = Just $ cofactorExpand 1 m | otherwise = Nothing
 
idm :: Fractional a => Int -> [[a]]
idm n = let xs = [1 .. n] in [fromIntegral . fromEnum . (x==) <$> xs | x <- xs]

inv :: (Ord a, Fractional a, Real a) => [[a]] -> Maybe [[a]]
inv m | all ((== length m) . length) m = ($ ls) $ fmap $ flip (zipWith (map . flip (/))) $ fromJust rs | otherwise = Nothing
    where
        ge = gaussianElim $ zipWith (++) m $ idm $ length m
        ls = unfoldr (\(i, xs) -> if length xs > i then Just ((xs !! i) !! i, (succ i, xs)) else Nothing) . (,) 0 <$> ge
        rs = map (drop (length (fromJust ge))) <$> ge

leastSquarePinv :: (Ord a, Fractional a, Real a) => [[a]] -> Maybe [[a]]
leastSquarePinv [] = Nothing
leastSquarePinv m 
    | all ((== length m) . length) m = inv m
    | length (head m) < length m = ($ inv (transpose m `mmul` m)) $ fmap (`mmul` transpose m)
    | otherwise = Nothing

leastSquarePinvRegular :: (Ord a, Fractional a, Real a) => a -> [[a]] -> Maybe [[a]]
leastSquarePinvRegular _ [] = Nothing
leastSquarePinvRegular l m
    | all ((== length m) . length) m = inv m
    | length (head m) < length m = ($ inv $ map (map (*l)) (idm (length m)) `madd` (transpose m `mmul` m)) $ fmap (`mmul` transpose m)
    | otherwise = Nothing
