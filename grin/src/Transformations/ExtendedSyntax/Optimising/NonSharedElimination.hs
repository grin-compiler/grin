{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Transformations.ExtendedSyntax.Optimising.NonSharedElimination where

{-
Remove the updates that update only non-shared locations.
-}

import Data.Maybe
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable

import Lens.Micro

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TypeEnv
import Transformations.ExtendedSyntax.Names (ExpChanges(..))
import AbstractInterpretation.ExtendedSyntax.Sharing.Result



nonSharedElimination :: SharingResult -> TypeEnv -> Exp -> (Exp, ExpChanges)
nonSharedElimination SharingResult{..} te exp = (exp', change) where

  exp' = cata skipUpdate exp

  change = if exp' /= exp then DeletedHeapOperation else NoChange

  -- Remove bind when the parameter points to non-shared locations only.
  skipUpdate :: ExpF Exp -> Exp
  skipUpdate = \case
    EBindF (SUpdate p _) _ rhs
      | all notShared . ptrLocations te $ p -> rhs
    exp -> embed exp

  notShared :: Loc -> Bool
  notShared l = not $ Set.member l _sharedLocs
