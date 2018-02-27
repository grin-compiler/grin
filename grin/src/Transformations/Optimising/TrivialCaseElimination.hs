{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.TrivialCaseElimination where

import Data.Functor.Foldable as Foldable
import Grin

trivialCaseElimination :: Exp -> Exp
trivialCaseElimination = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    ECase val [Alt cpat body] -> EBindF (SReturn val) lpat body where
      lpat = case cpat of
        NodePat tag args  -> ConstTagNode tag (map Var args)
        LitPat  lit       -> Lit lit
        TagPat  tag       -> ValTag tag

    exp -> project exp
