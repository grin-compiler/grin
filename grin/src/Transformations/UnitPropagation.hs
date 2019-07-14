{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Transformations.UnitPropagation where

import Transformations.Util
import Data.Functor.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Grin.Grin
import Grin.TypeEnv


{- QUESTION: Is this needed after the syntactical revamp?
   substVals can only change `pure`s
-}
unitPropagation :: TypeEnv -> Exp -> Exp
unitPropagation TypeEnv{..} e = e {-ana builder e where

  unitEnv :: Map Val Val
  unitEnv = Map.mapKeysMonotonic Var . flip Map.mapMaybe _variable $
    \ty -> if ty == unit_t then Just Unit else Nothing

  builder :: Exp -> ExpF Exp
  builder exp = let e = substVals unitEnv $ exp in case e of

    EBind leftExp lpat rightExp -> EBindF leftExp (substValsVal unitEnv lpat) rightExp

    _ -> project e
-}
