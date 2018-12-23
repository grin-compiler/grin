{-# LANGUAGE OverloadedStrings #-}
module Pipeline.Eval where

import qualified Data.Text.IO as Text
import Text.Megaparsec

import Grin.Grin
import Grin.TypeCheck
import Grin.Parse
import Reducer.Base (RTVal)
import qualified Reducer.IO
import qualified Reducer.Pure
import qualified Reducer.LLVM.JIT as LLVM
import qualified Reducer.LLVM.CodeGen as LLVM
import qualified AbstractInterpretation.HeapPointsTo.CodeGen as HPT
import qualified AbstractInterpretation.HeapPointsTo.Result as HPT
import AbstractInterpretation.Reduce (AbstractInterpretationResult(..), evalDataFlowInfo)
import AbstractInterpretation.IR (HasDataFlowInfo(..))



data Reducer
  = PureReducer
  | IOReducer
  | LLVMReducer
  deriving (Eq, Show)

-- TODO: Add Mode as a parameter?
eval' :: Reducer -> String -> IO RTVal
eval' reducer fname = do
  content <- Text.readFile fname
  case parseGrin fname content of
    Left err -> error $ parseErrorPretty' content err
    Right program ->
      case reducer of
        PureReducer -> Reducer.Pure.reduceFun program "grinMain"
        IOReducer   -> Reducer.IO.reduceFun program "grinMain"
        LLVMReducer -> LLVM.eagerJit (LLVM.codeGen typeEnv program) "grinMain" where
          typeEnv     = either error id $ typeEnvFromHPTResult =<< hptResult
          hptResult   = HPT.toHPTResult <$> hptProgram <*> ((_airComp . evalDataFlowInfo . getDataFlowInfo) <$> hptProgram)
          hptProgram  = HPT.codeGen program

evalProgram :: Reducer -> Program -> IO RTVal
evalProgram reducer program =
  case reducer of
    PureReducer -> Reducer.Pure.reduceFun program "grinMain"
    IOReducer   -> Reducer.IO.reduceFun program "grinMain"
