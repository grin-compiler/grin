{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module TypeEnv where

import Text.Printf
import Data.Int
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Map as Map

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

  -- dependent type constructions to describe deconstructed nodes
  | T_Tag         {_tagDomain   :: NodeSet}   -- tag with it's corresponding node set domain
  | T_Item        {_tagVariable :: Name       -- tag variable name that holds the node tag on which the item type depends
                  ,_itemIndex   :: Int        -- item index in the node
                  }
  deriving (Eq, Ord, Show)

data TypeEnv
  = TypeEnv
  { _location :: Vector NodeSet
  , _variable :: Map Name Type
  , _function :: Map Name (Type, Vector Type)
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''TypeEnv, ''Type, ''SimpleType]

variableType :: TypeEnv -> Name -> Type
variableType TypeEnv{..} name = case Map.lookup name _variable of
  Nothing -> error $ printf "variable %s is missing from type environment" name
  Just t -> t
