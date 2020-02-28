module Main where

import System.Environment (getArgs)
import CLI.Lib (mainWithArgs)


main :: IO ()
main = do
  args <- getArgs
  mainWithArgs args
