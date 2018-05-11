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

type Env = (Map Val Val, Map Name Name)

copyPropagation :: Exp -> Exp
copyPropagation e = hylo folder builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env@(valEnv, nameEnv), exp) = let e = substVarRefExp nameEnv $ exp in case e of
    -- left unit law
    EBind (SReturn val) lpat rightExp
      | newEnv <- env `mappend` unify env val lpat
      , reducedVal <- substNamesVal nameEnv val
      , constVal <- subst valEnv reducedVal
      -> case (lpat, constVal) of
        (ConstTagNode lpatTag lpatArgs, ConstTagNode valTag valArgs)
          | lpatTag == valTag
          , bindChain <- foldr (genBind env) rightExp $ zip valArgs lpatArgs
          -> (newEnv,) <$> project (EBind (SReturn Unit) Unit bindChain)
        _ -> (newEnv,) <$> project (genBind env (reducedVal, lpat) rightExp)

    _ -> (env,) <$> project e

  unify :: Env -> Val -> LPat -> Env
  unify env@(valEnv, nameEnv) (substNamesVal nameEnv -> val) lpat = case (lpat, val) of
    (ConstTagNode lpatTag lpatArgs, ConstTagNode valTag valArgs)
      | lpatTag == valTag         -> mconcat $ zipWith (unify env) valArgs lpatArgs
    (Var lpatVar, Var valVar)     -> (mempty, Map.singleton lpatVar valVar)
    (Var lpatVar, _)              -> (Map.singleton lpat val, mempty)
    _ -> mempty -- LPat: unit, lit, tag

  genBind :: Env -> (Val, LPat) -> Exp -> Exp
  genBind env@(valEnv, nameEnv) (val@(substValsVal valEnv -> constVal), lpat) exp = case lpat of
    ValTag{}        -> EBind (SReturn constVal) lpat exp
    Lit{}           -> EBind (SReturn constVal) lpat exp
    _               -> EBind (SReturn val) lpat exp

  -- QUESTION: does this belong here? or to dead variable elimination / constant propagation?
  folder :: ExpF Exp -> Exp
  folder = \case
    -- right unit law
    EBindF (SReturn val) lpat rightExp | val == lpat -> rightExp
    exp -> embed exp
