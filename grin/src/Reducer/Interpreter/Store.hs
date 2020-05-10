{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reducer.Interpreter.Store where

import Data.Maybe (fromMaybe)
import Grin.ExtendedSyntax.Pretty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- * Store

-- | Store maps addresses to abstract values.
newtype Store a v = Store (Map.Map a v)
  deriving (Eq, Ord, Show, Functor, Foldable)

empty :: (Ord a) => Store a v
empty = Store mempty

lookup :: (Ord a) => a -> Store a v -> v
lookup a (Store m) = fromMaybe (error "Store; missing") $ Map.lookup a m

insert :: (Ord a) => a -> v -> Store a v -> Store a v
insert a v (Store m) = Store (Map.insert a v m)

modify :: (Ord a) => a -> (v -> v) -> Store a v -> Store a v
modify a f (Store m) = Store (Map.adjust f a m)

size :: Store a v -> Int
size (Store m) = Map.size m

storeKeys :: Store a v -> Set.Set a
storeKeys (Store m) = Map.keysSet m

instance (Ord a, Semigroup v) => Semigroup (Store a v) where
  (Store ma) <> (Store mb) = Store (Map.unionWith (<>) ma mb)

instance (Ord a, Monoid v) => Monoid (Store a v) where
  mempty = Store mempty

instance (Pretty a, Pretty v) => Pretty (Store a v) where
  pretty (Store m) = prettyKeyValue (Map.toList m)
