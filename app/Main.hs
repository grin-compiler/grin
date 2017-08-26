module Main where

import Control.Monad
import System.Environment

import Eval

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> putStrLn "usage: grin GRIN_SOURCE"
    x   -> forM_ x $ \a -> eval' PureReducer a >>= print
