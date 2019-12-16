{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.ExtendedSyntax.Optimising.TrivialCaseElimination where

import Data.Functor.Foldable as Foldable
import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Util

trivialCaseElimination :: Exp -> Exp
trivialCaseElimination = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    ECase scrut [Alt DefaultPat _altName body] -> SBlockF body
    ECase scrut [Alt cpat       _altName body] -> SBlockF $ EBind (SReturn (Var scrut)) (cPatToAsPat scrut cpat) body
    exp -> project exp
