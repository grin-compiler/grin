{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Transformations.Optimising.DeadParameterElimination where

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec

import Data.List

import qualified Data.Foldable
import Data.Functor.Foldable as Foldable

import Control.Monad.Trans.Except

import Grin.Grin
import Grin.TypeEnvDefs
import Transformations.Util
import AbstractInterpretation.LiveVariable.Result as LVA

type Trf = Except String

runTrf :: Trf a -> Either String a
runTrf = runExcept

-- P and F nodes are handled by Dead Data Elimination
deadParameterElimination :: LVAResult -> TypeEnv -> Exp -> Either String Exp
deadParameterElimination lvaResult tyEnv = runTrf . cataM alg where
  alg :: ExpF Exp -> Trf Exp
  alg = \case
    DefF f args body -> do
      liveArgs <- onlyLiveArgs f args
      let deletedArgs = args \\ liveArgs
      body' <- bindToUndefineds tyEnv body deletedArgs
      return $ Def f liveArgs body'
    SAppF f args -> do
      liveArgs <- onlyLiveArgs f args
      return $ SApp f liveArgs
    e -> pure . embed $ e

  onlyLiveArgs :: Name -> [a] -> Trf [a]
  onlyLiveArgs f args = do
    argsLv <- lookupArgLivenessM f lvaResult
    return $ zipFilter args (Vec.toList argsLv)

lookupArgLivenessM :: Name -> LVAResult -> Trf (Vector Bool)
lookupArgLivenessM f LVAResult{..} = do
  let funNotFound = "Function " ++ show f ++ " was not found in liveness analysis result"
  (_,argLv) <- lookupExcept funNotFound f _functionLv
  return $ Vec.map isLive argLv
