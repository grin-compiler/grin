{-# LANGUAGE TypeApplications, LambdaCase, EmptyCase #-}
module Reducer.Interpreter.Definitional
  ( reduceFun
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Reducer.Interpreter.Base (Void, toExprF)
import Reducer.Interpreter.Definitional.Internal
import Reducer.Interpreter.Definitional.Instance
import Reducer.Base (RTVal(..))
import Reducer.Pure (EvalPlugin(..))
import Transformations.ExtendedSyntax.Conversion (convertToNew)
import qualified Grin.Syntax as SyntaxV1 (Exp, Name(..))


import qualified Data.Map as Map

reduceFun :: EvalPlugin -> SyntaxV1.Exp -> SyntaxV1.Name -> IO RTVal
reduceFun (EvalPlugin evalPrimOps) expV1 mainName = do
  (Left dval, _)
    <- evalDefinitional
        (DefinitionalTContext @Void @() @NoHeapInfo)
        (\case)
        (Map.map convertPrimOp $ Map.mapKeys nameV1toV2 evalPrimOps)
        (nameV1toV2 mainName)
        (toExprF $ convertToNew expV1)
  pure $ dValToRtVal dval
  where
    convertPrimOp f args = liftIO $ fmap rtValToDVal $ f $ map dValToRtVal args
