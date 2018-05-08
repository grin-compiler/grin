{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.Optimising.ConstantFolding where

import Text.Printf
import Transformations.Util
import Data.Functor.Foldable

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Grin
import Pretty

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

    EBind leftExp@(SReturn val) lpat rightExp -> (mappend env uniEnv,) <$> project bindExp where
      (binds, uniEnv) = unify env val lpat
      bindExp         = foldr (\(v, p) -> EBind (SReturn v) p) rightExp binds

    _ -> (env,) <$> project e

  unify :: Env -> Val -> LPat -> ([(Val, LPat)], Env)
  unify env@(nameEnv, valEnv) (subst valEnv -> val) lpat = case (lpat, val) of
    (ConstTagNode lpatTag lpatArgs, ConstTagNode valTag valArgs)
      | lpatTag == valTag     -> mconcat $ zipWith (unify env) valArgs lpatArgs
    (Var lpatVar, Var valVar) -> ([(val, lpat)], (Map.singleton lpatVar valVar, Map.singleton lpat val))  -- update val + name env
    (Var{}, _)                -> ([(val, lpat)], (mempty, Map.singleton lpat val))                        -- update val env
    _                         -> ([(val, lpat)], mempty) -- LPat: unit, lit, tag
