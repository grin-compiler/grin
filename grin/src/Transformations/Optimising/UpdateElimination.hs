{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.UpdateElimination where

import Data.Functor.Foldable as Foldable
import Grin

updateElimination :: Exp -> Exp
updateElimination = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    EBind   (SFetch  fetchLoc) fetchVal
     (EBind (SUpdate updateLoc updateVal) Unit rightExp)
      | fetchLoc == updateLoc &&
        fetchVal == updateVal
        -> EBindF (SFetch fetchLoc) fetchVal rightExp

    exp -> project exp
{-
  QUESTION: can this be subsumed by CSE which understands fetch/store/update?
-}
