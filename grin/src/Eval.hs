module Eval where

import Text.Megaparsec

import Grin
import ParseGrin
import qualified STReduceGrin
import qualified ReduceGrin


data Reducer
  = PureReducer
  | STReducer
  deriving (Eq, Show)

eval' :: Reducer -> String -> IO Val
eval' reducer fname = do
  result <- parseGrin fname
  case result of
    Left err -> error $ show err
    Right e  -> return $ 
      case reducer of
        PureReducer -> ReduceGrin.reduceFun e "grinMain"
        STReducer   -> STReduceGrin.reduceFun e "grinMain"

evalProgram :: Reducer -> Program -> Val
evalProgram reducer (Program defs) =
  case reducer of
    PureReducer -> ReduceGrin.reduceFun defs "grinMain"
    STReducer   -> STReduceGrin.reduceFun defs "grinMain"
