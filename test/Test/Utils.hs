module Test.Utils (
    runTest,
    getDiffFromFracLists,
    getRandSquareMatrix
) where


import Math.Matrix

import Control.Monad (void, replicateM)
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import Data.Tuple.Extra (first, second, dupe)
import System.Random
import System.IO (stderr)
import Test.HUnit (Test, runTestText, putTextToHandle)

runTest :: Test -> IO ()
runTest = void . runTestText (putTextToHandle stderr False) 

getDiffFromFracLists :: (Fractional a) => (a -> Bool) -> Matrix a -> Matrix a -> Maybe [[(a, a)]]
getDiffFromFracLists f l r = bool (Just ls) Nothing $ null ls
    where
        ls = filter (not . null) $ zipWith ((.) catMaybes . zipWith (\x y -> bool (Just (x, y)) Nothing $ f $ abs $ x - y)) l r

getRandSquareMatrix :: (Fractional a, Random a) => Int -> IO (Matrix a) 
getRandSquareMatrix = uncurry id . first replicateM . second (flip replicateM (getStdRandom (randomR (0, 0xff)))) . dupe
