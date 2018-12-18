{-# OPTIONS_GHC -Wall #-}
module Utils (
    PP (..),
    PPA (..),
    plot,
    plotAxisSpecified,
    implicitFn,
    help,
    s2i,
    mkFrames
) where

import Control.Monad.Fix (fix)
import Control.Exception (bracket)
import Control.Monad (when, void)
import qualified Graphics.Rendering.Chart.Backend.Cairo as GR
import qualified Graphics.Rendering.Chart.Easy as GR
import Graphics.Rendering.Chart.Axis.Types (PlotValue)
import System.Directory (listDirectory)
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.Process (readProcess)
import Data.List (sort, sortBy)
import Data.Char (isDigit)

import System.IO.Directory

s2i :: String -> Int
s2i = read

tmpname :: FilePath
tmpname = "tmp"

tmpdir :: FilePath
tmpdir = "tmpimg"

data PP a = PP FilePath String String String [(a, a)] (a -> a)
data PPA a = PPA (PP a) (a, a) (a, a)

help :: IO ()
help = flip (++) " <dta file path> <output image path> <degree number>" . (++) "Usage: " <$> getProgName >>= putStrLn >> exitFailure

execPlot :: (Ord a, Enum a, Fractional a) => PP a -> GR.EC (GR.Layout a a) ()
execPlot (PP _ title pt lt xyp yf) = do
    GR.layout_title GR..= title
    GR.setColors [GR.opaque GR.red, GR.opaque GR.blue]
    GR.plot $ GR.points pt xyp
    GR.plot $ GR.line lt [[(x, yf x) | x <- [fst $ head xs, 0.1 + fst (head xs) .. fst $ last xs]]]
    where
        xs = sort xyp

plot :: (Fractional a, Enum a, GR.PlotValue a) => PP a -> IO ()
plot (PP _ _ _ _ [] _) = return ()
plot pp@(PP fp _ _ _ _ _) = GR.toFile GR.def fp $ execPlot pp

plotAxisSpecified :: (RealFloat a, Show a, Enum a, GR.PlotValue a) => PPA a -> IO ()
plotAxisSpecified (PPA (PP _ _ _ _ [] _) _ _) = return ()
plotAxisSpecified (PPA pp@(PP fp _ _ _ _ _) (xmin, xmax) (ymin, ymax)) = GR.toFile GR.def fp $ do
    GR.layout_x_axis . GR.laxis_generate GR..= GR.scaledAxis GR.def (xmin `min` xmax, xmin `max` xmax) 
    GR.layout_y_axis . GR.laxis_generate GR..= GR.scaledAxis GR.def (ymin `min` ymax, ymin `max` ymax)
    execPlot pp

-- | Given matrix and return complete implicit function
implicitFn :: Fractional a => [[a]] -> Maybe (a -> a)
implicitFn [] = Nothing
implicitFn m 
    | any ((/=1) . length) m = Nothing 
    | otherwise = Just $ \x -> sum $ zipWith (*) (map head m) [f x | f <- map (flip (^^)) $ takeWhile (>=0) $ iterate (+(-1)) $ length m - 1]

-- | Given generating frame function, convert command path, data set, output path and degree and generate gif animation.
mkFrames :: (RealFloat a, Show a, Enum a, PlotValue a) => ([(a, a)] -> FilePath -> Int -> IO ()) -> FilePath -> [(a, a)] -> FilePath -> Int -> IO ()
mkFrames fn cv pt dst n = bracket (createDirectoryIfMiss tmpdir) removeDirIfExists $ \d -> do
    ($ 1) . fix $ \f i -> when (i <= n) $ absolutize (d ++ "/" ++ tmpname ++ show i ++ ".png") >>= flip (fn pt) i >> f (succ i)
    fs <- fmap (sortBy (\x y -> compare (s2i (filter isDigit x)) (s2i (filter isDigit y)))) . listDirectory =<< absolutize d
    if null fs then putStrLn "Failed to generate." >> exitFailure else
        mapM (absolutize . (++) (tmpdir ++ "/")) fs >>= void . flip (readProcess cv) [] . (++) ["-delay", "70"] . flip (++) [dst]

