{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module AbstractInterpretation.CByResultTypes where

import Data.Set    (Set)
import Data.Map    (Map)
import qualified Data.Set as S
import qualified Data.Map as M

import Lens.Micro.Platform
import Lens.Micro.Internal

import Grin.Grin (Name, Tag)
import AbstractInterpretation.HPTResult

-- possible producers grouped by tags
newtype ProducerMap = ProducerMap { _producerMap :: Map Name ProducerSet }
  deriving (Eq, Show)
newtype ProducerSet = ProducerSet { _producerSet :: Map Tag (Set Name)   }
  deriving (Eq, Show)

instance Monoid ProducerSet where
  mempty = ProducerSet M.empty
  mappend (ProducerSet x) (ProducerSet y) = ProducerSet $ M.unionWith mappend x y

instance Monoid ProducerMap where
  mempty = ProducerMap M.empty
  mappend (ProducerMap x) (ProducerMap y) = ProducerMap $ M.unionWith mappend x y

-- A graph representing the connections between producers.
-- p1 <-t-> p2 means: producers p1 and p2 share a consumer for tag t
-- In a ProducerMap, we map variables to producers,
-- in a ProducerGraph we map producers to other producers.
newtype ProducerGraph = ProducerGraph { _producerGraph :: ProducerMap }
  deriving (Eq, Show)

data GroupedProducers = All ProducerGraph | Active ProducerGraph

data CByResult
  = CByResult
  { _hptResult        :: HPTResult
  , _producers        :: ProducerMap
  , _groupedProducers :: GroupedProducers
  }

concat <$> mapM makeLenses [''ProducerMap, ''ProducerSet, ''CByResult, ''ProducerGraph]
