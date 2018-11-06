module Test.Matrix.Inverse (
    test,
    tests
) where

import Test.HUnit (Test (..), (@?=), (~:))
import Control.Monad (replicateM)
import Data.Tuple.Extra (first, second, dupe)
import Data.Maybe (maybe, fromMaybe)
import Data.Either (fromRight)
import qualified Data.Matrix as DM
import Linear.Epsilon (nearZero)

import Test.Utils
import Math.Matrix

test :: Int -> IO Test
test = fmap (("Test.Matrix.Inverse.test: " ~:) . maybe (True @?= True) (uncurry (@?=) . first (map (map fst)) . second (map (map snd)) . dupe) . uncurry (getDiffFromFracLists nearZero) . first (fromMaybe [[]] . inv) . second (DM.toLists . fromRight (DM.fromLists [[]]) . DM.inverse . DM.fromLists) . dupe) . (getRandSquareMatrix :: Int -> IO (Matrix Double))

tests :: Int -> Int -> IO [Test]
tests = flip (.) test . replicateM
