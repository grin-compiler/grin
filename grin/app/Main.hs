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
import qualified JITLLVM
import VarGen
import System.Process

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
      putStrLn . show . ondullred . pretty . registerIntroductionI 0 $ Program grin

      putStrLn "* bind normalisation / register introduction *"
      putStrLn . show . ondullcyan . pretty . bindNormalisation . registerIntroductionI 0 $ Program grin

      putStrLn "* original program *"
      printGrin $ Program grin

      -- grin code evaluation
      putStrLn "* evaluation result *"
      eval' PureReducer fname >>= print . pretty

      let (result, computer) = abstractRun (assignStoreIDs $ Program grin) "grinMain"
      putStrLn "* HPT *"
      print . pretty $ computer

      --putStrLn "* x86 64bit codegen *"
      --print . CGX64.codeGen $ Program grin

      --putStrLn "* LLVM codegen *"
      let mod = CGLLVM.codeGen $ Program grin
          llName = printf "%s.ll" fname
          sName = printf "%s.s" fname
      CGLLVM.toLLVM llName mod

      putStrLn "* LLVM X64 codegen *"
      callProcess "llc-5.0" [llName]
      readFile sName >>= putStrLn

      putStrLn "* LLVM JIT run *"
      JITLLVM.eagerJit mod "grinMain"
