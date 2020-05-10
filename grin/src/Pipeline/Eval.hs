{-# LANGUAGE OverloadedStrings #-}
module Pipeline.Eval where

import qualified Data.Text.IO as Text
import Text.Megaparsec

import Grin.Grin
import Grin.Parse
import Reducer.Base (RTVal, Statistics)
import qualified Reducer.IO
import qualified Reducer.Pure
import qualified Reducer.Interpreter.Definitional



data Reducer
  = PureReducer Reducer.Pure.EvalPlugin
  | IOReducer
  | DefinitionalReducer Reducer.Pure.EvalPlugin

evalProgram :: Reducer -> Program -> IO (RTVal, Maybe Statistics)
evalProgram reducer program =
  case reducer of
    PureReducer evalPrimOp
      -> Reducer.Pure.reduceFun evalPrimOp program "grinMain"
    IOReducer
      -> Reducer.IO.reduceFun program "grinMain"
    DefinitionalReducer evalPrimOp
      -> (\x -> (x,Nothing)) <$> Reducer.Interpreter.Definitional.reduceFun evalPrimOp program "grinMain"
