{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module TypeEnv where

import Data.Int
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
  | T_Location {_locations :: [Int]}
  deriving (Eq, Ord, Show)

type NodeSet = Map Tag (Vector SimpleType)

data Type
  = T_SimpleType  {_simpleType  :: SimpleType}
  | T_NodeSet     {_nodeSet     :: NodeSet}
  deriving (Eq, Ord, Show)

data TypeEnv
  = TypeEnv
  { _location :: Vector NodeSet
  , _variable :: Map Name Type
  , _function :: Map Name (Type, Vector Type)
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''TypeEnv, ''Type, ''SimpleType]
