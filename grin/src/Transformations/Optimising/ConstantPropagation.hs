{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.Optimising.ConstantPropagation where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import Grin
import Pretty
import Transformations.Util

type Env = Map Val Val

constantPropagation :: Exp -> Exp
constantPropagation e = ana builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env, exp) = case exp of
    ECase val alts ->
      let constVal      = subst env val
          known         = isKnown constVal || Map.member val env
          matchingAlts  = [alt | alt@(Alt cpat body) <- alts, match cpat constVal]
          defaultAlts   = [alt | alt@(Alt DefaultPat body) <- alts]
          -- HINT: use cpat as known value in the alternative ; bind cpat to val
          altEnv cpat   = env `mappend` unify env (cpatToLPat cpat) val
      in case (known, matchingAlts, defaultAlts) of
        -- known scutinee, specific pattern
        (True, [Alt cpat body], _)        -> (env,) <$> EBindF (SReturn constVal) (cpatToLPat cpat) body

        -- known scutinee, default pattern
        (True, _, [Alt DefaultPat body])  -> (env,) <$> EBindF (SReturn Unit) Unit body

        -- unknown scutinee
        -- HINT: in each alternative set val value like it was matched
        _ -> ECaseF val [(altEnv cpat, alt) | alt@(Alt cpat _) <- alts]

    -- track values
    EBind (SReturn val) lpat _rightExp -> (newEnv,) <$> project exp where
      newEnv = env `mappend` unify env val lpat

    _ -> (env,) <$> project exp

  unify :: Env -> Val -> LPat -> Env
  unify env (subst env -> val) lpat = case (lpat, val) of
    (ConstTagNode lpatTag lpatArgs, ConstTagNode valTag valArgs)
      | lpatTag == valTag     -> mconcat $ zipWith (unify env) valArgs lpatArgs
    (Var{}, _)                -> Map.singleton lpat val
    _                         -> mempty -- LPat: unit, lit, tag

  isKnown :: Val -> Bool
  isKnown = \case
    ConstTagNode{} -> True
    Lit{}          -> True
    ValTag{}       -> True
    _              -> False

  match :: CPat -> Val -> Bool
  match (NodePat tagA _) (ConstTagNode tagB _) = tagA == tagB
  match (LitPat litA)    (Lit litB)            = litA == litB
  match (TagPat tagA)    (ValTag tagB)         = tagA == tagB
  match _ _ = False
