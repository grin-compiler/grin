{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.ExtendedSyntax.Optimising.TrivialCaseElimination where

import Data.Functor.Foldable as Foldable
import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Util

trivialCaseElimination :: Exp -> Exp
trivialCaseElimination = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    ECase scrut [Alt DefaultPat altName body] -> SBlockF $ EBind (SReturn (Var scrut)) (VarPat altName)           body
    ECase scrut [Alt cpat       altName body] -> SBlockF $ EBind (SReturn (Var scrut)) (cPatToAsPat altName cpat) body
    exp -> project exp
