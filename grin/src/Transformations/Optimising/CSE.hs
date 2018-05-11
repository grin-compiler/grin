{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
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
commonSubExpressionElimination (typeEnv, e) = (typeEnv, hylo skipUnit builder (mempty, e)) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env, subst env -> exp) = case exp of
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
      extEnv = Map.insertWith (\new old -> new) leftExp (SReturn lpat) env
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
  altEnv env val cpat = case cpat of
    NodePat tag args  -> Map.insert (SReturn (ConstTagNode tag $ map Var args)) (SReturn val) env
    LitPat lit        -> Map.insert (SReturn (Lit lit)) (SReturn val) env
    TagPat tag        -> Map.insert (SReturn (ValTag tag)) (SReturn val) env
    DefaultPat        -> env
