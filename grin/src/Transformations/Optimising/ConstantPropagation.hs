{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.ConstantPropagation where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import Grin
import Pretty
import Transformations.Util

type Env = (Map Val Val)

-- TODO: handle returning case
constantPropagation :: Exp -> Exp
constantPropagation e = ana builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env, exp) = case substVals env exp of
    ECase val alts | isKnown val -> (env,) <$> EBindF (SReturn val) (cpatToLPat cpat) body where
      -- NOTE: there must be only one matching alternative!
      Alt cpat body = case [alt | alt@(Alt cpat body) <- alts, match cpat val] of
        [alt] -> alt
        _ -> error $ printf "illegal case expression\n%s" (show $ pretty exp)

    -- HINT: in each alternative set val value like it was matched
    ECase val alts -> ECaseF val [(Map.insert val (cpatToVal cpat) env, alt) | alt@(Alt cpat _) <- alts]

    -- track values
    EBind (SReturn val) lpat rightExp -> (Map.insert lpat (subst env val) env,) <$> project exp

    _ -> (env,) <$> project exp

  isKnown :: Val -> Bool
  isKnown = \case
    ConstTagNode{} -> True
    Lit{}          -> True
    ValTag{}       -> True
    _              -> False

  cpatToVal :: CPat -> Val
  cpatToVal = cpatToLPat

  match :: CPat -> Val -> Bool
  match (NodePat tagA _) (ConstTagNode tagB _) = tagA == tagB
  match (LitPat litA)    (Lit litB)            = litA == litB
  match (TagPat tagA)    (ValTag tagB)         = tagA == tagB
  match _ _ = False
