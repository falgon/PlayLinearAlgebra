{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Maybe (maybe)
import System.Exit (exitFailure)
import System.Environment (getArgs)

import Math.Approx.LEM.ByRecursion (linearEqs)
import System.IO.Dta
import Utils

main :: IO ()
main = do
    args <- getArgs
    if length args /= 3 then help else let [dta, op, degree] = args in 
        (>>=) (sequence [absolutize dta, absolutize op]) $ \[d, o] -> 
            (>>=) (readDta d) $ maybe exitFailure $ \xy -> ($ explicitFn $ linearEqs (read degree) xy) $ 
                maybe exitFailure $ plot . PP o "Figure" "points" "approximated line" xy
