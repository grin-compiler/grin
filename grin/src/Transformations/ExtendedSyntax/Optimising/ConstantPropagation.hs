{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.ConstantPropagation where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable

import Lens.Micro ((^.))

import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Util

{-
  HINT:
    propagates only tag values but not literals
    GRIN is not a supercompiler

  NOTE:
    We only need the tag information to simplify case expressions.
    This means that Env could be a Name -> Tag mapping.
-}

type Env = Map Name Val

constantPropagation :: Exp -> Exp
constantPropagation e = ana builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env, exp) = case exp of
    ECase scrut alts -> -- val ~ scrut
      let constVal      = getValue scrut env -- error "substValsVal env val"
          known         = isKnown constVal || Map.member scrut env
          matchingAlts  = [alt | alt@(Alt cpat name body) <- alts, match cpat constVal]
          defaultAlts   = [alt | alt@(Alt DefaultPat name body) <- alts]
          -- HINT: use cpat as known value in the alternative ; bind cpat to val
          altEnv cpat   = env `mappend` unify env scrut (cPatToVal cpat)
      in case (known, matchingAlts, defaultAlts) of
        -- known scutinee, specific pattern
        (True, [Alt cpat name body], _)        -> (env,) <$> SBlockF (EBind (SReturn $ constVal) (cPatToAsPat cpat name) body)

        -- known scutinee, default pattern
        (True, _, [Alt DefaultPat name body])  -> (env,) <$> SBlockF (EBind (SReturn $ Var scrut) (VarPat name) body)

        -- unknown scutinee
        -- HINT: in each alternative set val value like it was matched
        _ -> ECaseF scrut [(altEnv cpat, alt) | alt@(Alt cpat name _) <- alts]

    -- track values
    EBind (SReturn val) bPat rightExp -> (env `mappend` unify env (bPat ^. _BPatVar) val,) <$> project exp

    _ -> (env,) <$> project exp

  unify :: Env -> Name -> Val -> Env
  unify env var val = case val of
    ConstTagNode{}  -> Map.singleton var val
    Unit            -> Map.singleton var val -- HINT: default pattern (minor hack)
    Var v           -> Map.singleton var (getValue v env)
    Lit{}           -> mempty
    _               -> error $ "ConstantPropagation/unify: unexpected value: " ++ show (val) -- TODO: PP

  isKnown :: Val -> Bool
  isKnown = \case
    ConstTagNode{} -> True
    _              -> False

  match :: CPat -> Val -> Bool
  match (NodePat tagA _) (ConstTagNode tagB _) = tagA == tagB
  match _ _ = False

  getValue :: Name -> Env -> Val
  getValue varName env = Map.findWithDefault (Var varName) varName env
