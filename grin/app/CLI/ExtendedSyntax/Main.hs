module Main where

import System.Environment (getArgs)
import CLI.ExtendedSyntax.Lib (mainWithArgs)


main :: IO ()
main = do
  args <- getArgs
  mainWithArgs args
