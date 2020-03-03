{-# LANGUAGE OverloadedStrings #-}
module Pipeline.ExtendedSyntax.Eval where

import qualified Data.Text.IO as Text
import Text.Megaparsec

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TypeCheck
import Grin.ExtendedSyntax.Parse
import Reducer.ExtendedSyntax.Base (RTVal, Statistics)
import qualified Reducer.ExtendedSyntax.IO as ReducerIO
import qualified Reducer.ExtendedSyntax.Pure as ReducerPure
import qualified Reducer.ExtendedSyntax.LLVM.JIT as LLVM
import qualified Reducer.ExtendedSyntax.LLVM.CodeGen as LLVM
import qualified AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGen as HPT
import qualified AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result as HPT
import AbstractInterpretation.ExtendedSyntax.Reduce (AbstractInterpretationResult(..), evalAbstractProgram)



data Reducer
  = PureReducer ReducerPure.EvalPlugin
  | IOReducer
  | LLVMReducer

-- TODO: Add Mode as a parameter?
-- NOTE: this is used in benchmarks
eval' :: Reducer -> String -> IO RTVal
eval' reducer fname = do
  content <- Text.readFile fname
  case parseGrin fname content of
    Left err -> error $ errorBundlePretty err
    Right program ->
      case reducer of
        PureReducer evalPrimOp ->
          ReducerPure.reduceFunWithoutStats evalPrimOp program "grinMain"
        IOReducer ->
          ReducerIO.reduceFun program "grinMain"
        LLVMReducer -> LLVM.eagerJit (LLVM.codeGen typeEnv program) "grinMain" where
          typeEnv   = either error id $ typeEnvFromHPTResult hptResult
          hptResult = HPT.toHPTResult hptMapping ((_airComp . evalAbstractProgram) $ hptProgram)
          (hptProgram, hptMapping) = HPT.codeGen program

evalProgram :: Reducer -> Program -> IO (RTVal, Maybe Statistics)
evalProgram reducer program =
  case reducer of
    PureReducer evalPrimOp -> ReducerPure.reduceFun evalPrimOp program "grinMain"
    IOReducer              -> (\rtVal -> (rtVal, Nothing)) <$> ReducerIO.reduceFun program "grinMain"
