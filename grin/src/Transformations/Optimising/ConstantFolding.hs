{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.ConstantFolding where

import Text.Printf
import Transformations.Util
import Data.Functor.Foldable

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Grin
import Pretty

{-
Constant folding is not part of the official grin optimization pipeline.
This transformation is used for demonstrate and experiment with the
testing.
-}
type Env = (Map Name Name, Map Val Val)

constantFolding :: Exp -> Exp
constantFolding e = hylo skipUnit builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env@(nameEnv, valEnv), exp) = let e = substVals valEnv . substVarRefExp nameEnv $ exp in case e of

    EBind (SReturn val) lpat rightExp | Just newEnv <- unify env lpat val -> (mappend env newEnv,) <$> EBindF (SReturn Unit) Unit rightExp

    _ -> (env,) <$> project e

  unify :: Env -> LPat -> Val -> Maybe Env
  unify env@(nameEnv, valEnv) lpat val = case (lpat, val) of
    (ConstTagNode lpatTag lpatArgs, ConstTagNode valTag valArgs) ->
      if lpatTag /= valTag
        then error $ printf "mismatching tags, lpat: %s val: %s" (show $ pretty lpatTag) (show $ pretty valTag)
        else mconcat $ zipWith (unify env) lpatArgs valArgs

    (Var lpatVar, Var valVar) -> Just (Map.singleton lpatVar $ subst nameEnv valVar, mempty)  -- update name env
    (Var{}, _) -> Just (mempty, Map.singleton lpat $ subst valEnv val)                        -- update val env

    -- bypass otherwise
    _ -> Nothing
