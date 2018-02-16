{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module AbstractInterpretation.HPTResultNew where

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Lens.Micro.Platform

import Grin (Name, Tag)

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Unit
  deriving (Eq, Ord, Show)

data LocationOrSimpleType
  = Location    Int
  | SimpleType  SimpleType
  deriving (Eq, Ord, Show)

newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag (Vector (Set LocationOrSimpleType))} deriving (Eq, Ord, Show)

data TypeSet
  = TypeSet
  { _simpleTypeAndLocationSet :: Set LocationOrSimpleType
  , _nodeSet                  :: NodeSet
  }
  deriving (Eq, Ord, Show)

instance Monoid NodeSet where
  mempty  = NodeSet mempty
  mappend = unionNodeSet

instance Monoid TypeSet where
  mempty  = TypeSet mempty mempty
  mappend = unionTypeSet

unionNodeSet :: NodeSet -> NodeSet -> NodeSet
unionNodeSet (NodeSet x) (NodeSet y) = NodeSet $ Map.unionWith unionNodeData x y where
  unionNodeData a b
    | V.length a == V.length b = V.zipWith Set.union a b
    | otherwise = error $ "node arity mismatch " ++ show (V.length a) ++ " =/= " ++ show (V.length b)

unionTypeSet :: TypeSet -> TypeSet -> TypeSet
unionTypeSet a b = TypeSet
  { _simpleTypeAndLocationSet = Set.union (_simpleTypeAndLocationSet a) (_simpleTypeAndLocationSet b)
  , _nodeSet                  = unionNodeSet (_nodeSet a) (_nodeSet b)
  }

data HPTResult
  = HPTResult
  { _memory   :: Vector NodeSet
  , _register :: Map Name TypeSet
  , _function :: Map Name (TypeSet, Vector TypeSet)
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''NodeSet, ''TypeSet, ''HPTResult]

toLocValue :: Int32 -> LocationOrSimpleType
toLocValue ty | ty < 0 = SimpleType $ case ty of
  -1 -> T_Unit
  -2 -> T_Int64
  -3 -> T_Word64
  -4 -> T_Float
  -5 -> T_Bool
  _ -> error $ "unknown type code " ++ show ty
toLocValue l = Location $ fromIntegral l

