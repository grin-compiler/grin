{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Grin.TypeEnvDefs where

import Data.Data
import Data.Map (Map)
import Data.Vector (Vector)

import Data.Monoid

import Control.DeepSeq
import GHC.Generics (Generic)

import Lens.Micro.Platform

import Grin.SyntaxDefs

type NodeSet = Map Tag (Vector SimpleType)

data Type
  = T_SimpleType  {_simpleType  :: SimpleType}
  | T_NodeSet     {_nodeSet     :: NodeSet}

  -- dependent type constructions to describe deconstructed nodes
  | T_Tag         {_tagDomain   :: NodeSet}   -- tag with it's corresponding node set domain
  | T_Item        {_tagVariable :: Name       -- tag variable name that holds the node tag on which the item type depends
                  ,_itemIndex   :: Int        -- item index in the node
                  }
  deriving (Generic, Data, NFData, Eq, Ord, Show)

data TypeEnv
  = TypeEnv
  { _location :: Vector NodeSet
  , _variable :: Map Name Type
  , _function :: Map Name (Type, Vector Type)
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''TypeEnv, ''Type, ''SimpleType]

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv mempty mempty mempty
