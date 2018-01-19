{-# LANGUAGE LambdaCase #-}
module Transformations.AssignStoreIDs where

import Data.Functor.Foldable as Foldable

import Control.Comonad.Cofree
import Control.Monad.Gen

import Grin

type GenM = Gen Integer

-- Assign Store IDs
assignStoreIDs :: Exp -> Cofree ExpF Int
assignStoreIDs = runGen . cata folder where
  folder = \case
    SStoreF v -> (:< SStoreF v) <$> gen
    e -> (0 :<) <$> sequence e
