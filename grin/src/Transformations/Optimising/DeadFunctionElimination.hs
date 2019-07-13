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


type Trf = Except String

runTrf :: Trf a -> Either String a
runTrf = runExcept

deadFunctionElimination :: LVAResult -> TypeEnv -> Exp -> Either String Exp
deadFunctionElimination lvaResult tyEnv = runTrf .
  (deleteDeadFunctions lvaResult >=> replaceDeadFunApps lvaResult tyEnv)

deleteDeadFunctions :: LVAResult ->  Exp -> Trf Exp
deleteDeadFunctions lvaResult (Program exts defs) =
  fmap (Program exts) $ filterM isFunDefLiveM defs where

    isFunDefLiveM :: Exp -> Trf Bool
    isFunDefLiveM (Def f _ _) = fmap not $ isRemovableM lvaResult f
    isFunDefLiveM e = throwE $ "DFE: " ++ show (PP e) ++ " is not a function definition"


replaceDeadFunApps :: LVAResult -> TypeEnv -> Exp -> Trf Exp
replaceDeadFunApps lvaResult tyEnv = cataM alg where

  alg :: ExpF Exp -> Trf Exp
  alg = replaceAppWithUndefined lvaResult tyEnv . embed

replaceAppWithUndefined :: LVAResult -> TypeEnv -> Exp -> Trf Exp
replaceAppWithUndefined lvaResult TypeEnv{..} app@(SApp f _) = do
  isRemovable <- isRemovableM lvaResult f
  if isRemovable then do
    (retTy,_) <- lookupExcept (notFoundInTyEnv f) f _function
    pure $ SReturn $ Undefined (simplifyType retTy)
  else
    pure app
  where notFoundInTyEnv f = "DFE: Function " ++ show (PP f) ++ " not found in type env"
replaceAppWithUndefined _ _ e = pure e

isRemovableM :: LVAResult -> Name -> Trf Bool
isRemovableM lvaResult f = (&&) <$> isFunDeadM lvaResult f
                                <*> hasNoSideEffectsM lvaResult f

hasNoSideEffectsM :: LVAResult -> Name -> Trf Bool
hasNoSideEffectsM LVAResult{..} f = fmap (not . _hasEffect)
                                  . lookupExcept (noLiveness f) f
                                  $ _functionEff

isFunDeadM :: LVAResult -> Name -> Trf Bool
isFunDeadM LVAResult{..} f = fmap isFunDead
                           . lookupExcept (noLiveness f) f
                           $ _functionLv

noEffect   f = "DFE: Function " ++ show (PP f) ++ " not found in effect map"
noLiveness f = "DFE: Function " ++ show (PP f) ++ " not found in liveness map"
