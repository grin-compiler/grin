{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.DeadVariableElimination where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin
import TypeEnv
import Transformations.Util

deadVariableElimination :: (TypeEnv, Exp) -> (TypeEnv, Exp)
deadVariableElimination (typeEnv, e) = (typeEnv, fst $ cata folder e) where
  folder :: ExpF (Exp, Set Name) -> (Exp, Set Name)
  folder = \case
    EBindF _ lpat right@(_, rightRef)
      | lpat /= Unit
      , vars <- foldNamesVal Set.singleton lpat
      , all ((/=) unit_t . variableType typeEnv) vars
      , all (flip Set.notMember rightRef) vars
      -> right
    exp -> (embed $ fmap fst exp, foldNameUseExpF Set.singleton exp `mappend` Data.Foldable.fold (fmap snd exp))
