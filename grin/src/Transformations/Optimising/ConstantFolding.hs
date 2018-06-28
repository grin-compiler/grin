{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.Optimising.ConstantFolding where

import Text.Printf
import Transformations.Util
import Data.Functor.Foldable

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Grin.Grin

{-
HINT:
  Constant folding is not part of the official grin optimization pipeline because it causes problems with confluency.
  However it could be useful for debugging purposes.

IDEA: fold everything unconditionally
-}

type Env = (Map Name Name, Map Val Val)

constantFolding :: Exp -> Exp
constantFolding e = ana builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env@(nameEnv, valEnv), exp) = let e = substVals valEnv . substVarRefExp nameEnv $ exp in case e of

    EBind (SReturn val) lpat rightExp -> EBindF (env, SReturn $ subst valEnv val) lpat (newEnv, rightExp) where
      newEnv = env `mappend` unify env val lpat

    _ -> (env,) <$> project e

  unify :: Env -> Val -> LPat -> Env
  unify env@(nameEnv, valEnv) (subst valEnv -> val) lpat = case (lpat, val) of
    (ConstTagNode lpatTag lpatArgs, ConstTagNode valTag valArgs)
      | lpatTag == valTag     -> mconcat $ zipWith (unify env) valArgs lpatArgs
    (Var lpatVar, Var valVar) -> (Map.singleton lpatVar valVar, Map.singleton lpat val) -- update val + name env
    (Var{}, _)                -> (mempty, Map.singleton lpat val)                       -- update val env
    _                         -> mempty -- LPat: unit, lit, tag
