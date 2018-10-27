{-# OPTIONS_GHC -Wall #-}
module Utils (
    absolutize,
    PP (..),
    PPA (..),
    plot,
    plotAxisSpecified,
    explicitFn,
    Points,
    Matrix,
    help
) where

import qualified Graphics.Rendering.Chart.Backend.Cairo as GR
import qualified Graphics.Rendering.Chart.Easy as GR
import System.Environment (getProgName)
import System.IO (FilePath)
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory)
import System.Exit (exitFailure)
import Data.Maybe (fromJust)
import Data.List (sort, isPrefixOf)

import Math.Matrix (Matrix)

absolutize :: FilePath -> IO FilePath
absolutize p 
    | "~" `isPrefixOf` p = normalise . flip (++) (tail p) . addTrailingPathSeparator <$> getHomeDirectory
    | otherwise = fromJust . guess_dotdot <$> absolute_path p

type Points a = [(a, a)]

data PP a = PP FilePath String String String (Points a) (a -> a)
data PPA a = PPA (PP a) (a, a) (a, a)

help :: IO ()
help = flip (++) " <dta file path> <output image path> <degree number>" . (++) "Usage: " <$> getProgName >>= putStrLn >> exitFailure

plot :: (Fractional a, Enum a, GR.PlotValue a) => PP a -> IO ()
plot (PP _ _ _ _ [] _) = return ()
plot (PP fp title pt lt xyp yf) = GR.toFile GR.def fp $ do
    GR.layout_title GR..= title
    GR.setColors [GR.opaque GR.red, GR.opaque GR.blue]
    GR.plot $ GR.points pt xyp 
    GR.plot $ GR.line lt [[(x, yf x) | x <- [fst $ head xs, 0.1 + fst (head xs) .. fst $ last xs]]]
    where
        xs = sort xyp

plotAxisSpecified :: (RealFloat a, Show a, Enum a, GR.PlotValue a) => PPA a -> IO ()
plotAxisSpecified (PPA (PP _ _ _ _ [] _) _ _) = return ()
plotAxisSpecified (PPA (PP fp title pt lt xyp yf) (xmin, xmax) (ymin, ymax)) = GR.toFile GR.def fp $ do
    GR.layout_title GR..= title
    GR.layout_x_axis . GR.laxis_generate GR..= GR.scaledAxis GR.def (xmin `min` xmax, xmin `max` xmax) 
    GR.layout_y_axis . GR.laxis_generate GR..= GR.scaledAxis GR.def (ymin `min` ymax, ymin `max` ymax)
    GR.setColors [GR.opaque GR.red, GR.opaque GR.blue]
    GR.plot $ GR.points pt xyp
    GR.plot $ GR.line lt [[(x, yf x) | x <- [fst $ head xs, 0.1 + fst (head xs) .. fst $ last xs]]]
    where
        xs = sort xyp

explicitFn :: Fractional a => Matrix a -> Maybe (a -> a)
explicitFn [] = Nothing
explicitFn m 
    | any ((/=1) . length) m = Nothing 
    | otherwise = Just $ \x -> sum $ zipWith (*) (map head m) [f x | f <- map (flip (^^)) $ takeWhile (>=0) $ iterate (+(-1)) $ length m - 1]
