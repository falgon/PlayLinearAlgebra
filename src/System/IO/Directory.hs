{-# OPTIONS_GHC #-}
module System.IO.Directory (
    absolutize,
    createDirectoryIfMiss,
    removeDirIfExists
) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import System.IO (FilePath)
import System.Path.NameManip (guess_dotdot, absolute_path)
import System.FilePath (addTrailingPathSeparator, normalise)
import System.Directory (getHomeDirectory, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)

absolutize :: FilePath -> IO FilePath
absolutize p 
    | "~" `isPrefixOf` p = normalise . flip (++) (tail p) . addTrailingPathSeparator <$> getHomeDirectory
    | otherwise = fromJust . guess_dotdot <$> absolute_path p

createDirectoryIfMiss :: FilePath -> IO String
createDirectoryIfMiss fp = createDirectoryIfMissing False fp >> return fp

removeDirIfExists :: FilePath -> IO ()
removeDirIfExists fp = absolutize fp >>= doesDirectoryExist >>= flip when (removeDirectoryRecursive fp)
