{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Transformations.UnitPropagation where

import Transformations.Util
import Data.Functor.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Grin.Grin
import Grin.TypeEnv


unitPropagation :: TypeEnv -> Exp -> Exp
unitPropagation TypeEnv{..} e = ana builder e where

  -- Take all the variables, and delete everything besides
  -- those that have Unit type.
  unitEnv :: Map Val Val
  unitEnv = Map.mapKeysMonotonic Var . flip Map.mapMaybe _variable $
    \ty -> if ty == unit_t then Just Unit else Nothing

  -- Replace all occurences of variables with Unit type
  -- with the Unit value in all Vals (and only in Vals) in a given expression.
  builder :: Exp -> ExpF Exp
  builder exp = let e = substVals unitEnv $ exp in case e of

    EBind leftExp lpat rightExp -> EBindF leftExp (substValsVal unitEnv lpat) rightExp

    _ -> project e
