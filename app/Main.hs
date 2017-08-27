{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import System.Environment
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen (pretty)

import Eval
import ParseGrin
import Grin
import Pretty
import Transformations


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: grin GRIN_SOURCE"
    --x   -> forM_ x $ \a -> eval' PureReducer a >>= print
    x -> forM_ x $ \fname -> do
      grin <- either (fail . show) id <$> parseGrin fname
      let result = [printf "stores %s %d" name $ countStores exp | Def name _ exp <- grin]
      putStrLn $ unlines result
      putStrLn . show . pretty . vectorisation $ Program grin
      putStrLn . show . collectTagInfoPure $ Program grin
      putStrLn . show . pretty . generateEval $ Program grin
      printGrin grin
