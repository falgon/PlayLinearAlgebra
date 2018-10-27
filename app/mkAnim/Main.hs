{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Exception (bracket)
import Control.Monad.Fix (fix)
import Control.Monad (when, mapM, void)
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Maybe (maybe)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeDirectoryRecursive, findExecutable)
import System.Process (readProcess)

import Math.Approx.LEM.ByRecursion (linearEqs)
import System.IO.Dta
import Utils

tmpname :: FilePath
tmpname = "tmp"

tmpdir :: FilePath
tmpdir = "tmpimg"

msg :: String
msg = "This application needs the `convert` command of imagemagick. Please setup on your system."

s2i :: String -> Int
s2i = read

mkFrame :: FilePath -> FilePath -> Int -> IO ()
mkFrame dst src n = (>>=) (sequence [absolutize src, absolutize dst]) $ \[d, o] -> 
    (>>=) (readDta d) $ maybe exitFailure $ \xy -> ($ explicitFn $ linearEqs n xy) $ maybe exitFailure $ 
        plotAxisSpecified . 
            flip (flip PPA (minimum $ map fst xy, 1 + maximum (map fst xy))) (-50 + minimum (map snd xy), 50 + maximum (map snd xy)) . 
                PP o ("m = " ++ show n) "Data set" "Approximated line" xy
        
createDirectoryIfMiss :: FilePath -> IO String
createDirectoryIfMiss fp = createDirectoryIfMissing False fp >> return fp

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists fp = absolutize fp >>= doesDirectoryExist >>= flip when (removeDirectoryRecursive fp)

main :: IO ()
main = (>>=) (findExecutable "convert") $ maybe (putStrLn msg >> exitFailure) $ \cv -> do
    args <- getArgs
    if length args /= 3 then help else let [src, dst, ns] = args; n = read ns in bracket (createDirectoryIfMiss tmpdir) removeDirIfExists $ \d -> do
        ($ 1) . fix $ \f i -> when (i <= n) $ absolutize (d ++ "/" ++ tmpname ++ show i ++ ".png") >>= flip (flip mkFrame src) i >> f (succ i) 
        fs <- fmap (sortBy (\x y -> compare (s2i (filter isDigit x)) (s2i (filter isDigit y)))) . listDirectory =<< absolutize d
        if null fs then putStrLn "Failed to generate." >> exitFailure else 
            mapM (absolutize . (++) (d ++ "/")) fs >>= void . flip (readProcess cv) [] . (++) ["-delay", "70"] . flip (++) [dst]
