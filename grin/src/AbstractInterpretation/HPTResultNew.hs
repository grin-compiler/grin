{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving #-}
module AbstractInterpretation.HPTResultNew where

import Data.Int
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

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

newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag (Vector (Set LocationOrSimpleType))} deriving (Eq, Ord, Monoid, Show)

data Value
  = Value
  { _simpleTypeAndLocationSet :: Set LocationOrSimpleType
  , _nodeSet                  :: NodeSet
  }
  deriving (Eq, Ord, Show)

data HPTResult
  = HPTResult
  { _memory   :: Vector NodeSet
  , _register :: Map Name Value
  , _function :: Map Name (Value, Vector Value)
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''NodeSet, ''Value, ''HPTResult]

toLocValue :: Int32 -> LocationOrSimpleType
toLocValue ty | ty < 0 = SimpleType $ case ty of
  -1 -> T_Unit
  -2 -> T_Int64
  -3 -> T_Word64
  -4 -> T_Float
  -5 -> T_Bool
  _ -> error $ "unknown type code " ++ show ty
toLocValue l = Location $ fromIntegral l

