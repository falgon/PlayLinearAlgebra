{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Maybe (maybe, fromJust)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.Directory (findExecutable)
import Graphics.Rendering.Chart.Axis.Types (PlotValue)

import ML.Approx.OLS.ByLU (resolve)
import System.IO.Dta
import System.IO.Directory
import Utils

msg :: String
msg = "This application needs the `convert` command of imagemagick. Please setup on your system."

frame :: (RealFloat a, Show a, Enum a, PlotValue a) => [(a, a)] -> FilePath -> Int -> IO ()
frame [] _ _ = return ()
frame xy dst n = (>>=) (absolutize dst) $ \o -> ($ implicitFn $ fromJust $ resolve n xy) $ maybe exitFailure $ plotAxisSpecified . 
    flip (flip PPA (minimum $ map fst xy, 1 + maximum (map fst xy))) (-50 + minimum (map snd xy), 50 + maximum (map snd xy)) . 
        PP o ("m = " ++ show n) "Data set" "Approximated line" xy

main :: IO ()
main = (>>=) (findExecutable "convert") $ maybe (putStrLn msg >> exitFailure) $ \cv -> do
    args <- getArgs
    if length args /= 3 then help else let [src, dst, ns] = args; n = read ns in 
        (>>=) (absolutize src >>= readDta) $ maybe exitFailure $ flip (flip (mkFrames frame cv) dst) n
