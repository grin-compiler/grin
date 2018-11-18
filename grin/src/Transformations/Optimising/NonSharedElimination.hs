{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.NonSharedElimination where

{-
Remove the updates that update only non-shared locations.
-}

import Grin.Grin
import Grin.TypeEnv
import Data.Functor.Foldable as Foldable
import Lens.Micro
import Data.Maybe
import qualified Data.Set as Set



nonSharedElimination :: (TypeEnv, Exp) -> (TypeEnv, Exp)
nonSharedElimination (te, exp) = (te, cata skipUpdate exp) where

  skipUpdate :: ExpF Exp -> Exp
  skipUpdate = \case
    -- Remove bind when the parameter points to non-shared locations only.
    EBindF (SUpdate v _) _ rhs
      | not $ or $ concat $ te ^.. variable . at v . _Just . _T_SimpleType . _T_Location . to (map isShared) -> rhs
    exp -> embed exp

  isShared :: Loc -> Bool
  isShared l = Set.member l (_sharing te)
