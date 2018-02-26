{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Simplifying.CaseSimplification where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')

import Data.Functor.Foldable as Foldable

import Grin
import Transformations.Util

caseSimplification :: Exp -> Exp
caseSimplification e = ana builder (mempty, e) where
  builder :: (Map Val Val, Exp) -> ExpF (Map Val Val, Exp)
  builder (env, exp) =
    case exp of
      ECase (VarTagNode tagVar vals mapping {-TODO-}) alts -> ECaseF (subst env $ Var tagVar) (map (substAlt env vals) alts)
      e -> (env,) <$> project (substVals env e)

  substAlt env vals = \case
      Alt (NodePat tag vars) e -> (altEnv, Alt (TagPat tag) e)
                             where altEnv = foldl' (\m (name,val) -> Map.insert (Var name) (subst env val) m) env (zip vars vals)
      alt -> (env, alt)
