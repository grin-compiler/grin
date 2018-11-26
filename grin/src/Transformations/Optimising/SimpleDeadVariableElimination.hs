{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.SimpleDeadVariableElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Grin.Grin
import Grin.TypeEnv
import Grin.EffectMap
import Transformations.Util

-- TODO: Write for dead code elimination.
simpleDeadVariableElimination :: (TypeEnv, EffectMap, Exp) -> (TypeEnv, EffectMap, Exp)
simpleDeadVariableElimination (typeEnv, EffectMap effects, e) = (typeEnv, EffectMap effects, fst $ cata folder e) where

  folder :: ExpF (Exp, Set Name) -> (Exp, Set Name)
  folder = \case

    exp@(EBindF (left, _) lpat right@(_, rightRef))
      | lpat /= Unit
      , vars <- foldNamesVal Set.singleton lpat
      , all ((/=) unit_t . variableType typeEnv) vars
      , all (flip Set.notMember rightRef) vars
      -> case left of
          (SApp name _) | Just _ <- Map.lookup name effects -> embedExp exp
          SBlock{}  -> embedExp exp
          _         -> right

    exp -> embedExp exp
    where
      embedExp :: ExpF (Exp, Set Name) -> (Exp, Set Name)
      embedExp exp0 = (embed $ fmap fst exp0, foldNameUseExpF Set.singleton exp0 `mappend` Data.Foldable.fold (fmap snd exp0))
