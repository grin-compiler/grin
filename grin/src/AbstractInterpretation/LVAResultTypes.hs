{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module AbstractInterpretation.LVAResultTypes where

import Data.Map (Map)
import Data.Vector (Vector)

import Lens.Micro.Platform

import Grin.Grin (Name, Tag)
import AbstractInterpretation.LiveVariable (LVAProgram(..))
import AbstractInterpretation.IR as IR hiding (Tag, Liveness)

data Node = Node
  { _tag    :: Bool
  , _fields :: Vector Bool
  }
  deriving (Eq, Ord, Show)

data Liveness = BasicVal Bool
              | NodeSet (Map Tag Node)
  deriving (Eq, Ord, Show)

data LVAResult
  = LVAResult
  { _memory   :: Vector Liveness
  , _register :: Map Name Liveness
  , _function :: Map Name (Liveness, Vector Liveness)
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''Node, ''Liveness, ''LVAResult]
