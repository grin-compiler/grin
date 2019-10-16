{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Transformations.ExtendedSyntax.UnitPropagation where

import Data.Functor.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TypeEnv
import Transformations.ExtendedSyntax.Util


unitPropagation :: TypeEnv -> Exp -> Exp
unitPropagation TypeEnv{..} e = ana builder e where

  unitEnv :: Map Val Val
  unitEnv = Map.mapKeysMonotonic Var . flip Map.mapMaybe _variable $
    \ty -> if ty == unit_t then Just Unit else Nothing

  builder :: Exp -> ExpF Exp
  builder exp = let e = substVals unitEnv $ exp in case e of

    EBind leftExp lpat rightExp -> EBindF leftExp (substValsVal unitEnv lpat) rightExp

    _ -> project e
