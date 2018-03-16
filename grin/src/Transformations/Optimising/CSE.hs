{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.CSE where

-- HINT: common sub-expression elimination

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import Grin
import TypeEnv
import Pretty
import Transformations.Util

type Env = (Map SimpleExp SimpleExp)

-- TODO: track if function parameters with location type can be updated in the called function to improve CSE

commonSubExpressionElimination :: (TypeEnv, Exp) -> (TypeEnv, Exp)
commonSubExpressionElimination (typeEnv, e) = (typeEnv, ana builder (mempty, e)) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env, exp) = case exp of
    EBind leftExp lpat rightExp -> EBindF (env, leftExp) lpat (newEnv, rightExp) where
      newEnv = case leftExp of
        -- HINT: also save fetch (the inverse operation) for store and update
        SUpdate name val              -> Map.insert (SFetch name) (SReturn val) env
        SStore val | Var name <- lpat -> Map.insert (SFetch name) (SReturn val) extEnv
        -- HINT: location parameters might be updated in the called function, so forget their content
        SApp _defName args            -> foldr Map.delete extEnv [SFetch name | Var name <- args, isLocation name]
        SReturn{} -> extEnv
        SFetch{}  -> extEnv
        _         -> env
      extEnv = Map.insertWith const leftExp (SReturn lpat) env
    _ -> (env,) <$> project (subst env exp)

  isLocation :: Name -> Bool
  isLocation name = case variableType typeEnv name of
    T_SimpleType T_Location{} -> True
    _ -> False
