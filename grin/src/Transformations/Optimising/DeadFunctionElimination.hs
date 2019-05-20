{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Transformations.Optimising.DeadFunctionElimination where

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec

import Data.List
import Data.Maybe
import Data.Monoid

import qualified Data.Foldable
import Data.Functor.Foldable as Foldable

import Lens.Micro
import Lens.Micro.Platform

import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Trans.Except

import Grin.Grin
import Grin.Pretty
import Grin.TypeEnv
import Transformations.Util
import AbstractInterpretation.LiveVariable.Result as LVA
import AbstractInterpretation.EffectTracking.Result as ET


type Trf = Except String

runTrf :: Trf a -> Either String a
runTrf = runExcept

deadFunctionElimination :: LVAResult -> ETResult -> TypeEnv -> Exp -> Either String Exp
deadFunctionElimination lvaResult etResult tyEnv = runTrf .
  (deleteDeadFunctions lvaResult etResult >=> replaceDeadFunApps lvaResult etResult tyEnv)

deleteDeadFunctions :: LVAResult -> ETResult -> Exp -> Trf Exp
deleteDeadFunctions lvaResult etResult (Program exts defs) =
  fmap (Program exts) $ filterM isFunDefLiveM defs where

    isFunDefLiveM :: Exp -> Trf Bool
    isFunDefLiveM (Def f _ _) = fmap not $ isFunDeadM lvaResult etResult f
    isFunDefLiveM e = throwE $ "DFE: " ++ show (PP e) ++ " is not a function definition"


replaceDeadFunApps :: LVAResult -> ETResult -> TypeEnv -> Exp -> Trf Exp
replaceDeadFunApps lvaResult etResult tyEnv = cataM alg where

  alg :: ExpF Exp -> Trf Exp
  alg = replaceAppWithUndefined lvaResult etResult tyEnv . embed

replaceAppWithUndefined :: LVAResult -> ETResult -> TypeEnv -> Exp -> Trf Exp
replaceAppWithUndefined lvaResult etResult TypeEnv{..} app@(SApp f _) = do
  funIsDead <- isFunDeadM lvaResult etResult f
  if funIsDead then do
    (retTy,_) <- lookupExcept (notFoundInTyEnv f) f _function
    pure $ SReturn $ Undefined (simplifyType retTy)
  else
    pure app
  where notFoundInTyEnv f = "DFE: Function " ++ show (PP f) ++ " not found in type env"
replaceAppWithUndefined _ _ _ e = pure e


isFunDeadM :: LVAResult -> ETResult -> Name -> Trf Bool
isFunDeadM LVAResult{..} etResult f = fmap andHasNoSideEffect
                                    . fmap isFunDead
                                    . lookupExcept (noLiveness f) f
                                    $ _function
  where andHasNoSideEffect = (&&) (not $ hasSideEffectFun etResult f)

noLiveness f = "DFE: Function " ++ show (PP f) ++ " not found in liveness map"
