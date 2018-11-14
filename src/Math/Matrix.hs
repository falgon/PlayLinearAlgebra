{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
module Math.Matrix (
    Matrix,
    resolveLinearEq,
    determinant,
    madd,
    mmul,
    inv,
    pinv,
    l2Reg,
    transpose
) where

import Data.Maybe (fromJust)
import Data.List (transpose, unfoldr)
import Data.Tuple.Extra (first, second, dupe)

#ifdef __GLASGOW_HASKELL__
{-# INLINE swapElems #-}
{-# INLINE madd #-}
{-# INLINE mmul #-}
{-# INLINE idm #-}
#endif

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

forward :: [[Rational]] -> Maybe [[Rational]]
forward = forward' 0
    where
        forward' n xs
            | length xs - 1 > n = maybe Nothing (forward' (succ n)) $ toZero n (setMaxAbs n xs)
            | otherwise = Just xs
        toZero i xs 
            | ((xs !! i) !! i) == 0 = Nothing
            | otherwise = Just $ take (succ i) xs ++ [let ur = xs !! i; c = (r !! i) / (ur !! i) in zipWith ((+) . negate . (*c)) ur r | r <- drop (i + 1) xs]

backward :: [[Rational]] -> Maybe [[Rational]]
backward = uncurry backward' . first ((+(-1)) . length) . dupe
    where
        backward' n xs
            | n >= 0 = maybe Nothing (backward' (pred n)) $ toZero n xs
            | otherwise = Just xs
        toZero i xs 
            | ((xs !! i) !! i) == 0 = Nothing
            | otherwise = Just $ [let ur = xs !! i; c = (r !! i) / (ur !! i) in zipWith ((+) . negate . (*c)) ur r | r <- take i xs] ++ drop i xs

gaussianElim :: [[Rational]] -> Maybe [[Rational]]
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

madd :: [[Rational]] -> [[Rational]] -> [[Rational]]
madd = zipWith (zipWith (+))

mmul :: [[Rational]] -> [[Rational]] -> [[Rational]]
mmul lm rm = [[sum $ zipWith (*) l r | r <- transpose rm] | l <- lm]

resolveLinearEq :: [[Rational]] -> [Rational] -> Maybe [Rational]
resolveLinearEq = (.) (fmap (map (uncurry (/) . first last . second (sum . init) . dupe))) . (.) gaussianElim . zipWith (flip (.) (:[]) . (++))

determinant :: Num a => [[a]] -> Maybe a
determinant m | all ((== length m) . length) m = Just $ cofactorExpand 1 m | otherwise = Nothing

idm :: Int -> [[Rational]]
idm n = let xs = [1 .. n] in [fromIntegral . fromEnum . (x==) <$> xs | x <- xs]

inv :: [[Rational]] -> Maybe [[Rational]]
inv m | all ((== length m) . length) m = ($ ls) $ fmap $ flip (zipWith (map . flip (/))) $ fromJust rs | otherwise = Nothing
    where
        ge = gaussianElim $ zipWith (++) m $ idm $ length m
        ls = unfoldr (\(i, xs) -> if length xs > i then Just ((xs !! i) !! i, (succ i, xs)) else Nothing) . (,) 0 <$> ge
        rs = map (drop (length (fromJust ge))) <$> ge

pinv :: [[Rational]] -> Maybe [[Rational]]
pinv [] = Nothing
pinv m 
    | all ((== length m) . length) m = inv m
    | all ((< length m) . length) m = (`mmul` transpose m) <$> inv (transpose m `mmul` m)
    | all ((> length m) . length) m = (transpose m `mmul`) <$> inv (m `mmul` transpose m)
    | otherwise = Nothing

l2Reg :: (Fractional a, Real a) => a -> [(a, a)] -> Maybe [[Rational]]
l2Reg _ [] = Nothing
l2Reg l m = fmap (`mmul` transpose m') $ inv $ map (map (* toRational l)) (idm (length m')) `madd` (transpose m' `mmul` m')
    where
        m' = map (++[1]) $ transpose $ unfoldr (\i -> if i /= 0 then Just (toRational . (^^i) . fst <$> m, pred i) else Nothing) $ length m - 1
