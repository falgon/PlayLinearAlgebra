{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Maybe (maybe)
import Data.Random.Normal (normalIO')
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.Directory (findExecutable)
import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import Math.Approx.LEM.ByRecursion (linearEqs)
import System.IO.Directory
import Data.Random.Sample (sample)
import Utils

help' :: IO ()
help' = flip (++) " <function name> <output image path> <degree number>" . (++) "Usage: " <$> getProgName >>= putStrLn >> exitFailure

msg :: String
msg = "This application needs the `convert` command of imagemagick. Please setup on your system."

frame :: (RealFloat a, Show a, Enum a, PlotValue a) => [(a, a)] -> FilePath -> Int -> IO ()
frame pt dst n = do
    fname <- absolutize dst
    ($ implicitFn $ linearEqs n pt) $ maybe exitFailure $ plotAxisSpecified . 
        flip (flip PPA (minimum $ map fst pt, maximum (map fst pt))) (-0.5 + minimum (map snd pt), 0.5 + maximum (map snd pt)) .
            PP fname ("m = " ++ show n) "Data set" "Approximated line" pt
            
main :: IO ()
main = (>>=) (findExecutable "convert") $ maybe (putStrLn msg >> exitFailure) $ \cv -> do
    args <- getArgs
    dat <- sample 11 0 sin (normalIO' (0, 0.2)) :: IO [(Double, Double)]
    if length args /= 2 then help' else let [dst, ns] = args; n = read ns in mkFrames frame cv dat dst n
