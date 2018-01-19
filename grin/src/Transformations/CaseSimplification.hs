{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.CaseSimplification where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')

import Data.Functor.Foldable as Foldable

import Grin

type SubstMap = Map SimpleVal SimpleVal

mapVals :: (Val -> Val) -> Exp -> Exp
mapVals f = \case
  ECase val alts -> ECase (f val) alts
  SApp name vals -> SApp name (map f vals)
  SReturn val -> SReturn $ f val
  SStore val -> SStore $ f val
  SUpdate name val -> SUpdate name $ f val
  exp -> exp

substExpVals :: SubstMap -> Exp -> Exp
substExpVals env = mapVals (subst env)

subst env x = Map.findWithDefault x x env

caseSimplification :: Exp -> Exp
caseSimplification e = ana builder (mempty, e) where
  builder :: (SubstMap, Exp) -> ExpF (SubstMap, Exp)
  builder (env, exp) =
    case exp of
      ECase (VarTagNode tagVar vals) alts -> ECaseF (subst env $ Var tagVar) (map (substAlt env vals) alts)
      e -> (env,) <$> project (substExpVals env e)

  substAlt env vals = \case
      Alt (NodePat tag vars) e -> (altEnv, Alt (TagPat tag) e)
                             where altEnv = foldl' (\m (name,val) -> Map.insert (Var name) (subst env val) m) env (zip vars vals)
      alt -> (env, alt)
