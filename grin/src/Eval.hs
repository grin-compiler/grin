module Eval where

import Text.Megaparsec

import Grin
import ParseGrin
import qualified Reducer.ST
import qualified Reducer.Pure
import qualified Reducer.LLVM.JIT as LLVM
import qualified Reducer.LLVM.CodeGen as LLVM
import qualified AbstractInterpretation.AbstractRunGrin as Abstract
import Transformations.AssignStoreIDs (assignStoreIDs)

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
          (result, hptResult) = Abstract.abstractRun (assignStoreIDs $ Program e) "grinMain"

evalProgram :: Reducer -> Program -> Val
evalProgram reducer (Program defs) =
  case reducer of
    PureReducer -> Reducer.Pure.reduceFun defs "grinMain"
    STReducer   -> Reducer.ST.reduceFun defs "grinMain"
