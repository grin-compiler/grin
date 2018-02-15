module Eval where

import Text.Megaparsec

import Grin
import ParseGrin
import qualified Reducer.ST
import qualified Reducer.Pure
import qualified Reducer.LLVM.JIT as LLVM
import qualified Reducer.LLVM.CodeGen as LLVM
import qualified AbstractInterpretation.CodeGen as HPT
import qualified AbstractInterpretation.Reduce as HPT

data Reducer
  = PureReducer
  | STReducer
  | LLVMReducer
  deriving (Eq, Show)

eval' :: Reducer -> String -> IO Val
eval' reducer fname = do
  content <- readFile fname
  case parseGrin fname content of
    Left err -> error $ parseErrorPretty' content  err
    Right e  ->
      case reducer of
        PureReducer -> pure $ Reducer.Pure.reduceFun e "grinMain"
        STReducer   -> pure $ Reducer.ST.reduceFun e "grinMain"
        LLVMReducer -> LLVM.eagerJit (LLVM.codeGen hptResult (Program e)) "grinMain" where
          hptResult = HPT.toHPTResult hptProgram $ HPT.evalHPT hptProgram
          hptProgram = HPT.codeGen (Program e)

evalProgram :: Reducer -> Program -> Val
evalProgram reducer (Program defs) =
  case reducer of
    PureReducer -> Reducer.Pure.reduceFun defs "grinMain"
    STReducer   -> Reducer.ST.reduceFun defs "grinMain"
