{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Maybe (maybe)
import System.Exit (exitFailure)
import System.Environment (getArgs, getProgName)

import ML.Approx.Regularization.L2 (resolve)
import System.IO.Dta
import System.IO.Directory (absolutize)
import Utils hiding (help)

help :: IO ()
help = flip (++) " <dta file path> <output image path> <parameter>" . (++) "Usage: " <$> getProgName >>= putStrLn >> exitFailure

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then help else let [dta, op, degree] = args in 
        (>>=) (sequence [absolutize dta, absolutize op]) $ \[d, o] -> 
            (>>=) (readDta d) $ maybe exitFailure $ \xy -> flip (maybe exitFailure) (resolve (read degree) xy) $
                maybe exitFailure (plot . PP o "Figure" "points" "approximated line" xy) . implicitFn
