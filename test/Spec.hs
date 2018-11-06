module Main where

import qualified Test.Matrix.Inverse as TMI
import Test.Utils
import Test.HUnit (Test (..))

main :: IO ()
main = TMI.tests 20 9 >>= runTest . TestList
