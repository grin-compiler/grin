{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Simplifying.CaseSimplification where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Functor.Foldable as Foldable

import Grin.Grin
import Transformations.Util

caseSimplification :: Exp -> Exp
caseSimplification e = ana builder (mempty, mempty, e) where
  builder :: (Map Val Val, Map Name Name, Exp) -> ExpF (Map Val Val, Map Name Name, Exp)
  builder (valEnv, nameEnv, exp) =
    case exp of
      ECase (VarTagNode tagVar vals) alts -> ECaseF (Var $ subst nameEnv tagVar) (map (buildAlt vals) alts)
      -- TODO: handle const tag node
      e -> (valEnv, nameEnv,) <$> project (substVals valEnv . substVarRefExp nameEnv $ e)

    where
      buildAlt vals = \case
        Alt (NodePat tag vars) e -> (altValEnv, altNameEnv, Alt (TagPat tag) e) where
          (altValEnv, altNameEnv) = foldl' add (valEnv, nameEnv) (zip vars vals)
          add (vEnv, nEnv) (name, val) = case val of
            Var n -> (vEnv, Map.insert name (subst nEnv n) nEnv)          -- name -> name substitution
            _     -> (Map.insert (Var name) (subst vEnv val) vEnv, nEnv)  -- Val -> Val substitution ; i.e. Var name -> Lit
        alt -> (valEnv, nameEnv, alt)
