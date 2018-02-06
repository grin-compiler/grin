{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.Reduce where

import Data.Vector (Vector)
import Data.Int
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import AbstractInterpretation.IR

type NodeItem = Set Int32
type NodeData = Vector NodeItem
type NodeSet = Map Tag NodeData

data Value
  = Value
  { simpleTypeSet :: Set Int32
  , locationSet   :: Set Int32
  , nodeSet       :: NodeSet
  }

data Computer
  = Computer
  { memory    :: Vector NodeSet
  , register  :: Vector Value
  }

