{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.HPTResultNew where

import Data.Int
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import Grin (Name, Tag)

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Unit
  deriving (Eq, Ord, Show)

data LocOrValue
  = Loc         Int32
  | SimpleType  SimpleType
  deriving (Eq, Ord, Show)

newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag (Vector (Set LocOrValue))} deriving (Eq, Show)

data Value
  = Value
  { _simpleTypeAndLocationSet :: Set LocOrValue
  , _nodeSet                  :: NodeSet
  }
  deriving (Eq, Show)

data HPTResult
  = HPTResult
  { _memory   :: Vector NodeSet
  , _register :: Map Name Value
  , _function :: Map Name (Value, Vector Value)
  }
  deriving (Eq, Show)

toLocValue :: Int32 -> LocOrValue
toLocValue ty | ty < 0 = SimpleType $ case ty of
  -1 -> T_Unit
  -2 -> T_Int64
  -3 -> T_Word64
  -4 -> T_Float
  -5 -> T_Bool
  _ -> error $ "unknown type code " ++ show ty
toLocValue l = Loc l

