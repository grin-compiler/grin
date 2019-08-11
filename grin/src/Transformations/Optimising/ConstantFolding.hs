{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.Optimising.ConstantFolding
{-# DEPRECATED "Constant folding makes no sense in the new syntactic framework" #-}
  where

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
constantFolding e = e {-ana builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env@(nameEnv, valEnv), exp) = let e = substVals valEnv . substVarRefExp nameEnv $ exp in case e of

    EBind (SReturn val) bPat rightExp -> EBindF (env, SReturn $ subst valEnv val) bPat (newEnv, rightExp) where
      newEnv = env `mappend` unify env val bPat

    _ -> (env,) <$> project e

  unify :: Env -> Val -> BPat -> Env
  unify env@(nameEnv, valEnv) (subst valEnv -> val) bPat = case (bPat, val) of
    (ConstTagNode bPatTag bPatArgs, ConstTagNode valTag valArgs)
      | bPatTag == valTag     -> mconcat $ zipWith (unify env) valArgs bPatArgs
    (Var lpatVar, Var valVar) -> (Map.singleton lpatVar valVar, Map.singleton bPat val) -- update val + name env
    (Var{}, _)                -> (mempty, Map.singleton bPat val)                       -- update val env
    _                         -> mempty -- LPat: unit, lit, tag
-}
