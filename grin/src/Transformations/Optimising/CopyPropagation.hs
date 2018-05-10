{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.Optimising.CopyPropagation where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import Grin
import Pretty
import Transformations.Util

{-
  NOTE:
    Do not propagate literal values because literals are not used for optimisations. (GRIN is not a supercompiler)
    Only propagates variables. It does not cause performance penalty, LLVM will optimise the code further.
-}

type Env = Map Name Name

copyPropagation :: Exp -> Exp
copyPropagation e = hylo folder builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (nameEnv, exp) = let e = substVarRefExp nameEnv $ exp in case e of
    -- left unit law
    EBind (SReturn val) lpat rightExp -> EBindF (nameEnv, SReturn $ substNamesVal nameEnv val) lpat (newEnv, rightExp) where
      newEnv = nameEnv `mappend` unify nameEnv val lpat

    _ -> (nameEnv,) <$> project e

  unify :: Env -> Val -> LPat -> Env
  unify nameEnv (substNamesVal nameEnv -> val) lpat = case (lpat, val) of
    (ConstTagNode lpatTag lpatArgs, ConstTagNode valTag valArgs)
      | lpatTag == valTag     -> mconcat $ zipWith (unify nameEnv) valArgs lpatArgs
    (Var lpatVar, Var valVar) -> Map.singleton lpatVar valVar
    _                         -> mempty -- LPat: unit, lit, tag

  -- question: does this belong here? or to dead variable elimination / constant propagation?
  folder :: ExpF Exp -> Exp
  folder = \case
    -- right unit law
    EBindF (SReturn val) lpat rightExp | val == lpat -> rightExp
    exp -> embed exp
