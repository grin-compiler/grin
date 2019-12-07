{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.Optimising.CSE where

-- HINT: common sub-expression elimination

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import Grin.Grin
import Grin.TypeEnv
import Grin.EffectMap
import Transformations.Util

import Debug.Trace

type Env = (Map SimpleExp SimpleExp)

-- TODO: track if function parameters with location type can be updated in the called function to improve CSE

commonSubExpressionElimination :: TypeEnv -> EffectMap -> Exp -> Exp
commonSubExpressionElimination typeEnv effMap e = hylo skipUnit builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env, subst env -> exp) = case exp of
    EBind leftExp lpat rightExp -> EBindF (env, leftExp) lpat (newEnv, rightExp) where
      newEnv = case leftExp of
        -- HINT: also save fetch (the inverse operation) for store and update
        SUpdate name val              -> Map.insert (SFetch name) (SReturn val) env
        SStore val | Var name <- lpat -> Map.insert (SFetch name) (SReturn val) extEnvKeepOld
        -- HINT: location parameters might be updated in the called function, so forget their content
        SApp defName args -> foldr
          Map.delete
          (if (hasTrueSideEffect defName effMap) then env else extEnvKeepOld)
          [SFetch name | Var name <- args, isLocation name]
        SReturn val | isConstant val  -> extEnvKeepOld
        SFetch{}  -> extEnvKeepOld
        _         -> env
      extEnvKeepOld = Map.insertWith (\new old -> old) leftExp (SReturn lpat) env
    SUpdate name val | Just (SReturn fetchedVal) <- Map.lookup (SFetch name) env
                     , fetchedVal == val
                     -> SReturnF Unit
    ECase val alts -> ECaseF val [(altEnv env val cpat, alt) | alt@(Alt cpat _) <- alts]
    _ -> (env,) <$> project exp

  isLocation :: Name -> Bool
  isLocation name = case variableType typeEnv name of
    T_SimpleType T_Location{} -> True
    _ -> False

  altEnv :: Env -> Val -> CPat -> Env
  altEnv env val cpat
    | not (isConstant val)
    = case cpat of
      NodePat tag args  -> env -- When we use scrutinee variable already HPT will include all the
                               -- possible values, instead of the matching one. As result it will
                               -- overapproximate the values more than needed.

                               -- NOTE: We could extend the env with [ SReturn (ConstTagNode tag args) -> SReturn val ]
                               -- HPT would _not_ overapproximate the possible type of the variable,
                               -- since it restricts the scrutinee to the alternative's domain
      LitPat lit        -> Map.insertWith (\new old -> old) (SReturn (Lit lit)) (SReturn val) env
      TagPat tag        -> Map.insertWith (\new old -> old) (SReturn (ValTag tag)) (SReturn val) env
      DefaultPat        -> env

    | otherwise = env
