{-# LANGUAGE DeriveFunctor #-}
module Reducer.Interpreter.Env where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Grin.ExtendedSyntax.Syntax
import Grin.ExtendedSyntax.Pretty
import qualified Data.Map.Strict as Map

-- * Env

-- | Environment mapping names to abstract values.
newtype Env v = Env (Map.Map Name v)
  deriving (Eq, Show, Ord, Functor)

empty :: Env v
empty = Env mempty

lookup :: (Env v) -> Name -> v
lookup (Env m) n = fromMaybe (error $ "Missing:" ++ show n) $ Map.lookup n m

insert :: Name -> v -> Env v -> Env v
insert n v (Env m) = Env $ Map.insert n v m

inserts :: [(Name, v)] -> Env v -> Env v
inserts vs (Env m) = Env $ foldl' (\n (k,v) -> Map.insert k v n) m vs

insertEnv :: Env v -> Env v -> Env v
insertEnv (Env old) (Env new) = Env (Map.unionWith (\_ n -> n) old new)

-- Explicit instance!! different from default
instance (Semigroup v) => Semigroup (Env v) where
  Env m1 <> Env m2 = Env (Map.unionWith (<>) m1 m2)

instance (Semigroup v) => Monoid (Env v) where
  mempty = Env mempty

instance (Pretty v) => Pretty (Env v) where
  pretty (Env m) = prettyKeyValue (Map.toList m)
