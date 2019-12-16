{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.Optimising.CopyPropagation where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable

import Text.Printf
import Lens.Micro.Extra

import Grin.Grin
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
    EBindF leftExp lpat (SReturn val) | val == lpat, isn't _ValVar lpat -> leftExp

    -- left unit law ; cleanup matching constants
    EBindF (SReturn val) lpat rightExp
      | val == lpat
      , isConstant val
      -> rightExp
    -- left unit law ; cleanup x <- pure y copies
    {- NOTE: This case could be handled by SDVE as well, however
       performing it locally saves us an effect tracking analysis.
       This is because here, we have more information about variable
       bidnings. We know for sure that such copying bindings are not needed
       since all the occurences of the left-hand side have been replaced with
       the variable on the right-hand side.
    -}
    EBindF (SReturn Var{}) Var{} rightExp
      -> rightExp

    exp -> embed exp
