module Pipeline.Eval where

import Text.Megaparsec

import Grin.Grin
import Grin.TypeCheck
import Grin.Parse
import Reducer.Base (RTVal)
import qualified Reducer.IO
import qualified Reducer.Pure
import qualified Reducer.LLVM.JIT as LLVM
import qualified Reducer.LLVM.CodeGen as LLVM
import qualified AbstractInterpretation.CodeGenMain as HPT
import qualified AbstractInterpretation.Reduce as HPT

data Reducer
  = PureReducer
  | IOReducer
  | LLVMReducer
  deriving (Eq, Show)

-- TODO: Add Mode as a parameter?
eval' :: Reducer -> String -> IO RTVal
eval' reducer fname = do
  content <- readFile fname
  case parseGrin fname content of
    Left err -> error $ parseErrorPretty' content err
    Right program ->
      case reducer of
        PureReducer -> Reducer.Pure.reduceFun program "grinMain"
        IOReducer   -> Reducer.IO.reduceFun program "grinMain"
        LLVMReducer -> LLVM.eagerJit (LLVM.codeGen typeEnv program) "grinMain" where
          typeEnv     = either error id $ typeEnvFromHPTResult =<< hptResult
          hptResult   = HPT.toHPTResult <$> hptProgram <*> (fst . HPT.evalHPT <$> hptProgram)
          hptProgram  = HPT.codeGen program

evalProgram :: Reducer -> Program -> IO RTVal
evalProgram reducer program =
  case reducer of
    PureReducer -> Reducer.Pure.reduceFun program "grinMain"
    IOReducer   -> Reducer.IO.reduceFun program "grinMain"
