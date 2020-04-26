{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Transformations.ExtendedSyntax.Optimising.NonSharedElimination where

{-
Remove the updates that update only non-shared locations.
-}

import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TypeEnv (ptrLocations)
import Grin.ExtendedSyntax.TypeCheck (typeEnvFromHPTResult)
import Transformations.ExtendedSyntax.Names (ExpChanges(..))
import AbstractInterpretation.ExtendedSyntax.Sharing.Result (SharingResult(..))



nonSharedElimination :: SharingResult -> Exp -> (Exp, ExpChanges)
nonSharedElimination SharingResult{..} exp = (exp', change) where

  exp' = cata skipUpdate exp

  change = if exp' /= exp then DeletedHeapOperation else NoChange

  tyEnv = either error id $ typeEnvFromHPTResult _hptResult

  -- Remove bind when the parameter points to non-shared locations only.
  skipUpdate :: ExpF Exp -> Exp
  skipUpdate = \case
    EBindF (SUpdate p _) _ rhs
      | all notShared . ptrLocations tyEnv $ p -> rhs
    exp -> embed exp

  notShared :: Loc -> Bool
  notShared l = not $ Set.member l _sharedLocs
