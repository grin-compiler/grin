{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.Optimising.ConstantPropagation where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import Grin.Grin
import Transformations.Util

type Env = Map Val Val

{-
  HINT:
    propagates only tag values but not literals
    GRIN is not a supercompiler
-}

constantPropagation :: Exp -> Exp
constantPropagation e = ana builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env, exp) = case exp of
    ECase val alts ->
      let constVal      = substValsVal env val
          known         = isKnown constVal || Map.member val env
          matchingAlts  = [alt | alt@(Alt cpat body) <- alts, match cpat constVal]
          defaultAlts   = [alt | alt@(Alt DefaultPat body) <- alts]
          -- HINT: use cpat as known value in the alternative ; bind cpat to val
          altEnv cpat   = env `mappend` unify env (cpatToLPat cpat) val
      in case (known, matchingAlts, defaultAlts) of
        -- known scutinee, specific pattern
        (True, [Alt cpat body], _)        -> (env,) <$> SBlockF (EBind (SReturn constVal) (cpatToLPat cpat) body)

        -- known scutinee, default pattern
        (True, _, [Alt DefaultPat body])  -> (env,) <$> SBlockF body

        -- unknown scutinee
        -- HINT: in each alternative set val value like it was matched
        _ -> ECaseF val [(altEnv cpat, alt) | alt@(Alt cpat _) <- alts]

    -- track values
    EBind (SReturn val) lpat rightExp -> (env `mappend` unify env val lpat,) <$> project exp

    _ -> (env,) <$> project exp

  unify :: Env -> Val -> LPat -> Env
  unify env (substValsVal env -> val) lpat = case (lpat, val) of
    (Var{}, ConstTagNode{})   -> Map.singleton lpat val
    (Var{}, Unit)             -> Map.singleton lpat val -- HINT: default pattern (minor hack)
    _                         -> mempty -- LPat: unit, lit, tag

  isKnown :: Val -> Bool
  isKnown = \case
    ConstTagNode{} -> True
    ValTag{}       -> True
    _              -> False

  match :: CPat -> Val -> Bool
  match (NodePat tagA _) (ConstTagNode tagB _) = tagA == tagB
  match (TagPat tagA)    (ValTag tagB)         = tagA == tagB
  match _ _ = False
