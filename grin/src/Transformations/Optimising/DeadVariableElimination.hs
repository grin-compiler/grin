{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.DeadVariableElimination where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin
import Transformations.Util

deadVariableElimination :: Exp -> Exp
deadVariableElimination = fst . cata folder where
  folder :: ExpF (Exp, Set Name) -> (Exp, Set Name)
  folder = \case
    EBindF _ lpat right@(_, rightRef)
      | lpat /= Unit
      , vars <- foldNamesVal Set.singleton lpat
      , all (flip Set.notMember rightRef) vars
      -> right
    exp -> (embed $ fmap fst exp, foldNameUseExpF Set.singleton exp `mappend` Data.Foldable.fold (fmap snd exp))
