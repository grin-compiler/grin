{-# LANGUAGE LambdaCase #-}
module Transformations.CountVariableUse where

import Data.Functor.Foldable as Foldable
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Foldable

import Transformations.Util

import Grin.Grin


countVariableUse :: Exp -> Map Name Int
countVariableUse exp = appEndo (cata folder exp) mempty where
  folder e = Data.Foldable.fold e `mappend` foldNameUseExpF (\name -> Endo $ Map.unionWith (+) $ Map.singleton name 1) e

nonlinearVariables :: Exp -> Set Name
nonlinearVariables = Map.keysSet . Map.filter (>1) . countVariableUse
