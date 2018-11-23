{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.NonSharedElimination where

{-
Remove the updates that update only non-shared locations.
-}

import Data.Functor.Foldable as Foldable
import Lens.Micro
import Data.Maybe
import qualified Data.Set as Set

import Grin.Grin
import Grin.TypeEnv
import AbstractInterpretation.Sharing



nonSharedElimination :: SharingResult -> TypeEnv -> Exp -> Exp
nonSharedElimination shLocs te = cata skipUpdate where

  -- Remove bind when the parameter points to non-shared locations only.
  skipUpdate :: ExpF Exp -> Exp
  skipUpdate = \case
    EBindF (SUpdate p _) _ rhs
      | all notShared . ptrLocations te $ p -> rhs
    exp -> embed exp

  notShared :: Loc -> Bool
  notShared l = not $ Set.member l shLocs
