{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Main where

import Control.Monad
import System.Environment
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Eval
import ParseGrin
import Grin
import Pretty
import PrettyHPT
import Transformations
import TrafoPlayground
import AbstractRunGrin
import qualified CodeGenX64 as CGX64
import qualified CodeGenLLVM as CGLLVM
import VarGen

import Data.IntMap as IntMap
import Data.Map as Map

pipeline :: Exp -> Exp
pipeline =
  registerIntroductionM 0 .
  renameVaribales (Map.fromList [("i'", "i''"), ("a", "a'")]) .
  generateEval

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: grin GRIN_SOURCE"
    x -> forM_ x $ \fname -> do
      grin <- either (fail . show) id <$> parseGrin fname
      let result = [printf "stores %s %d" name $ countStores exp | Def name _ exp <- grin]
      putStrLn "* store count *"
      putStrLn $ unlines result
      putStrLn "* tag info *"
      putStrLn . show . collectTagInfo $ Program grin

      putStrLn "* vectorisation / split fetch operation / case simplifiaction*"
      let optProgram = caseSimplification . splitFetch . vectorisation $ Program grin
      putStrLn . show . ondullblack . pretty $ optProgram
      --putStrLn "* evaluation result *"
      --print . pretty $ evalProgram PureReducer optProgram

      putStrLn "* generate eval / rename variables / register introduction *"
      putStrLn . show . ondullmagenta . pretty . pipeline $ Program grin

      putStrLn "* register introduction *"
      putStrLn . show . ondullred . pretty . registerIntroduction 0 $ Program grin

      putStrLn "* bind normalisation / register introduction *"
      putStrLn . show . ondullcyan . pretty . bindNormalisation . registerIntroduction 0 $ Program grin

      putStrLn "* original program *"
      printGrin $ Program grin

      -- grin code evaluation
      putStrLn "* evaluation result *"
      eval' PureReducer fname >>= print . pretty

      let (result, computer) = abstractRun (assignStoreIDs $ Program grin) "main"
      putStrLn "* HPT *"
      print . pretty $ computer

      putStrLn "* x86 64bit codegen *"
      print . CGX64.codeGen $ Program grin

      putStrLn "* LLVM codegen *"
      CGLLVM.printLLVM $ Program grin
