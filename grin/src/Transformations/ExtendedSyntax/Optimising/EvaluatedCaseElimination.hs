{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.ExtendedSyntax.Optimising.EvaluatedCaseElimination where

import Data.Functor.Foldable as Foldable
import Grin.ExtendedSyntax.Grin

evaluatedCaseElimination :: Exp -> Exp
evaluatedCaseElimination = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    ECase scrut alts | all (altBodyEQ $ SReturn (Var scrut)) alts -> SReturnF (Var scrut)
    exp -> project exp

  altBodyEQ :: Exp -> Alt -> Bool
  altBodyEQ exp (Alt _cpat _altName body) = exp == body

