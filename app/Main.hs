{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import System.Environment
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Eval
import ParseGrin
import Grin
import Pretty
import Transformations
import AbstractRunGrin

import Data.Map as Map

pipeline :: Exp -> Exp
pipeline =
  renameVaribales (fromList [("i'", "i''"), ("a", "a'")]) .
  generateEval

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: grin GRIN_SOURCE"
    x -> forM_ x $ \fname -> do
      grin <- either (fail . show) id <$> parseGrin fname
      let result = [printf "stores %s %d" name $ countStores exp | Def name _ exp <- grin]
      putStrLn $ unlines result
      putStrLn . show . ondullblack . pretty . vectorisation $ Program grin
      putStrLn . show . ondullgreen . pretty . splitFetch . vectorisation $ Program grin
      putStrLn . show . collectTagInfoPure $ Program grin
      putStrLn . show . ondullblue . pretty . pipeline $ Program grin
      printGrin $ Program grin

      -- grin code evaluation
      eval' PureReducer fname >>= print . pretty

      print $ abstractRun grin "main"
