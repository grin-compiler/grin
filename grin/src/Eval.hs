module Eval where

import Text.Megaparsec

import Grin
import ParseGrin
import qualified STReduceGrin
import qualified ReduceGrin
import qualified JITLLVM
import qualified CodeGenLLVM
import qualified AbstractRunGrin
import Transformations (assignStoreIDs)

data Reducer
  = PureReducer
  | STReducer
  | LLVMReducer
  deriving (Eq, Show)

eval' :: Reducer -> String -> IO Val
eval' reducer fname = do
  result <- parseGrin fname
  case result of
    Left err -> error $ show err
    Right e  ->
      case reducer of
        PureReducer -> pure $ ReduceGrin.reduceFun e "grinMain"
        STReducer   -> pure $ STReduceGrin.reduceFun e "grinMain"
        LLVMReducer -> JITLLVM.eagerJit (CodeGenLLVM.codeGen hptResult (Program e)) "grinMain" where
          (result, hptResult) = AbstractRunGrin.abstractRun (assignStoreIDs $ Program e) "grinMain"

evalProgram :: Reducer -> Program -> Val
evalProgram reducer (Program defs) =
  case reducer of
    PureReducer -> ReduceGrin.reduceFun defs "grinMain"
    STReducer   -> STReduceGrin.reduceFun defs "grinMain"
