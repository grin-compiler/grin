{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Grin.ExtendedSyntax.TypeEnvDefs where

import Data.Binary

import Data.Data
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Vector as V (fromList, toList)

import Data.Monoid

import Control.DeepSeq
import GHC.Generics (Generic)

import Lens.Micro.Platform

import Grin.ExtendedSyntax.SyntaxDefs

-- TODO: put orphan instances into a separate module
instance Binary a => Binary (Vector a) where
  get = V.fromList <$> get
  put = put . V.toList

type NodeSet = Map Tag (Vector SimpleType)

data Type
  = T_SimpleType  {_simpleType  :: SimpleType}
  | T_NodeSet     {_nodeSet     :: NodeSet}
  deriving (Generic, Data, NFData, Binary, Eq, Ord, Show)

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
