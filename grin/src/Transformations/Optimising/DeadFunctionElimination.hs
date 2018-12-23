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
import Grin.EffectMap
import Transformations.Util
import AbstractInterpretation.LiveVariable.Result as LVA


type Trf = Except String

runTrf :: Trf a -> Either String a
runTrf = runExcept

deadFunctionElimination :: LVAResult -> EffectMap -> TypeEnv -> Exp -> Either String Exp
deadFunctionElimination lvaResult effMap tyEnv = runTrf .
  (deleteDeadFunctions lvaResult effMap >=> replaceDeadFunApps lvaResult effMap tyEnv)

deleteDeadFunctions :: LVAResult -> EffectMap -> Exp -> Trf Exp
deleteDeadFunctions lvaResult effMap (Program exts defs) =
  fmap (Program exts) $ filterM isFunDefLiveM defs where

    isFunDefLiveM :: Exp -> Trf Bool
    isFunDefLiveM (Def f _ _) = fmap not $ isFunDeadM lvaResult effMap f
    isFunDefLiveM e = throwE $ "DFE: " ++ show (PP e) ++ " is not a function definition"


replaceDeadFunApps :: LVAResult -> EffectMap -> TypeEnv -> Exp -> Trf Exp
replaceDeadFunApps lvaResult effMap tyEnv = cataM alg where

  alg :: ExpF Exp -> Trf Exp
  alg = replaceAppWithUndefined lvaResult effMap tyEnv . embed

replaceAppWithUndefined :: LVAResult -> EffectMap -> TypeEnv -> Exp -> Trf Exp
replaceAppWithUndefined lvaResult effMap TypeEnv{..} app@(SApp f _) = do
  funIsDead <- isFunDeadM lvaResult effMap f
  if funIsDead then do
    (retTy,_) <- lookupExcept (notFoundInTyEnv f) f _function
    pure $ SReturn $ Undefined (simplifyType retTy)
  else
    pure app
  where notFoundInTyEnv f = "DFE: Function " ++ show (PP f) ++ " not found in type env"
replaceAppWithUndefined _ _ _ e = pure e


isFunDeadM :: LVAResult -> EffectMap -> Name -> Trf Bool
isFunDeadM LVAResult{..} effMap f = fmap andHasNoSideEffect
                                  . fmap isFunDead
                                  . lookupExcept (noLiveness f) f
                                  $ _function
  where andHasNoSideEffect = (&&) (not $ hasTrueSideEffect f effMap)

noLiveness f = "DFE: Function " ++ show (PP f) ++ " not found in liveness map"