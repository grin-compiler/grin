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

simplifyForOptimization :: HPTResult -> Exp -> Exp
simplifyForOptimization hptResult =
  caseSimplification .
  vectorisation hptResult

simplifyForCodeGen :: Exp -> Exp
simplifyForCodeGen =
  registerIntroductionI 0 .
  rightHoistFetch .
  splitFetch

lowerGrin :: HPTResult -> Exp -> Exp
lowerGrin hptResult = simplifyForCodeGen . simplifyForOptimization hptResult

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

      let (result, hptResult) = abstractRun (assignStoreIDs $ Program grin) "grinMain"
      putStrLn "* HPT *"
      print . pretty $ hptResult

      putStrLn "* vectorisation / split fetch operation / case simplifiaction*"
      let optProgram = caseSimplification . splitFetch . vectorisation hptResult $ Program grin
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
      putStrLn "* Pure evaluation result *"
      eval' PureReducer fname >>= print . pretty
      putStrLn "* Right hoist fetch *"
      print $ rightHoistFetchCollect $ Program grin
      putStrLn . show . pretty $ Program grin
      putStrLn . show . pretty . rightHoistFetch $ Program grin

      --putStrLn "* x86 64bit codegen *"
      --print . CGX64.codeGen $ Program grin

      let lowGrin = bindNormalisation . lowerGrin hptResult $ Program grin
      putStrLn "* Low level GRIN *"
      putStrLn . show . ondullcyan . pretty $ lowGrin

      putStrLn "* LLVM codegen *"
      let mod = CGLLVM.codeGen lowGrin
          llName = printf "%s.ll" fname
          sName = printf "%s.s" fname
      print mod
      putStrLn "* to LLVM *"
      CGLLVM.toLLVM llName mod

      putStrLn "* LLVM X64 codegen *"
      callProcess "llc-5.0" [llName]
      readFile sName >>= putStrLn

      putStrLn "* LLVM JIT run *"
      JITLLVM.eagerJit mod "grinMain"
