{-# OPTIONS_GHC -Wall #-}
module System.IO.Dta (
    readDta
) where

import Control.Monad.Fix (fix)
import Data.Bool (bool)
import Data.Tuple.Extra (first, second, dupe)
import System.IO (IOMode (..), withFile, hGetLine, hIsEOF)

readDta :: String -> IO (Maybe [(Double, Double)])
readDta fname = withFile fname ReadMode $ \h -> ($ []) . fix $ \f s -> (>>=) (hIsEOF h) $ flip bool (return $ Just s) $ do
    line <- words <$> hGetLine h
    if length line == 2 then f $ (uncurry (,) . first head . second last . dupe . map (read :: String -> Double)) line : s else return Nothing
