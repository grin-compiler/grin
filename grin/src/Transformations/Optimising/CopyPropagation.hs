{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.CopyPropagation where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import Grin
import Pretty
import Transformations.Util

type Env = (Map Name Name, Map Val Val)

{-
  NOTE:
    Do not propagate literal values because literals are not used for optimisations. (GRIN is not a supercompiler)
    Only propagates variables. It does not cause performance penalty, LLVM will optimise the code further.
-}

copyPropagation :: Exp -> Exp
copyPropagation e = hylo folder builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env@(nameEnv, valEnv), exp) = let e = substVals valEnv . substVarRefExp nameEnv $ exp in case e of
    -- right unit law
    EBind leftExp valIn (SReturn valOut) | valIn == valOut -> (env,) <$> EBindF (SReturn Unit) Unit leftExp

    -- left unit law
    EBind (SReturn val) lpat rightExp | Just newEnv <- unify env lpat val -> (mappend env newEnv,) <$> EBindF (SReturn Unit) Unit rightExp

    _ -> (env,) <$> project e

  -- HINT: unify controls which (lpat/val) cases should be handled by copy propagation
  unify :: Env -> LPat -> Val -> Maybe Env
  unify env@(nameEnv, valEnv) lpat val = case (lpat, val) of
    (ConstTagNode lpatTag lpatArgs, ConstTagNode valTag valArgs) ->
      if lpatTag /= valTag
        then error $ printf "mismatching tags, lpat: %s val: %s" (show $ pretty lpatTag) (show $ pretty valTag)
        else mconcat $ zipWith (unify env) lpatArgs valArgs

    (Var{}, ConstTagNode{})   -> Just (mempty, Map.singleton lpat $ subst valEnv val)         -- update val env
    (Var lpatVar, Var valVar) -> Just (Map.singleton lpatVar $ subst nameEnv valVar, mempty)  -- update name env

    --  case A: Unit, Lit
    _ | lpat == val && isBasicValue lpat -> Just mempty -- HINT: nothing to do

    -- bypass otherwise
    _ -> Nothing

  folder :: ExpF Exp -> Exp
  folder = \case
    EBindF (SReturn Unit) Unit rightExp -> rightExp
    EBindF (SReturn (ConstTagNode valTag valArgs)) (ConstTagNode lpatTag lpatArgs) rightExp ->
      foldr (\(val, lpat) exp -> EBind (SReturn val) lpat exp) rightExp (zip valArgs lpatArgs)
    exp -> embed exp
