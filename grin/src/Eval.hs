module Eval where

import Text.Megaparsec

import Grin
import TypeCheck
import ParseGrin
import qualified Reducer.IO
import qualified Reducer.Pure
import qualified Reducer.LLVM.JIT as LLVM
import qualified Reducer.LLVM.CodeGen as LLVM
import qualified AbstractInterpretation.CodeGen as HPT
import qualified AbstractInterpretation.Reduce as HPT

data Reducer
  = PureReducer
  | IOReducer
  | LLVMReducer
  deriving (Eq, Show)

eval' :: Reducer -> String -> IO Val
eval' reducer fname = do
  content <- readFile fname
  case parseGrin fname content of
    Left err -> error $ parseErrorPretty' content  err
    Right program ->
      case reducer of
        PureReducer -> Reducer.Pure.reduceFun program "grinMain"
        IOReducer   -> Reducer.IO.reduceFun program "grinMain"
        LLVMReducer -> LLVM.eagerJit (LLVM.codeGen typeEnv program) "grinMain" where
          typeEnv     = typeEnvFromHPTResult hptResult
          hptResult   = HPT.toHPTResult hptProgram $ HPT.evalHPT hptProgram
          hptProgram  = HPT.codeGen program

evalProgram :: Reducer -> Program -> IO Val
evalProgram reducer program =
  case reducer of
    PureReducer -> Reducer.Pure.reduceFun program "grinMain"
    IOReducer   -> Reducer.IO.reduceFun program "grinMain"
