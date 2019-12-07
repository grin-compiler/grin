{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveGeneric #-}
module AbstractInterpretation.IR
 ( module AbstractInterpretation.IR
 , Int32
 , Word32
 , Name
 ) where

import Data.Int
import Data.Word

import Data.Functor.Foldable.TH

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import Data.Set (Set)

import Lens.Micro.Platform

import qualified Grin.Grin as Grin
import Grin.Grin (Name)
import GHC.Generics (Generic)
import Control.DeepSeq

newtype Reg = Reg Word32 deriving (Eq, Ord, Show)
newtype Mem = Mem Word32 deriving (Eq, Ord, Show)

data Selector
  = NodeItem              Tag Int   -- node item index
  | ConditionAsSelector   Condition
  | AllFields
  | EveryNthField         Int
  deriving (Eq, Ord, Show)

newtype Tag = Tag Word32 deriving (Eq, Ord, Show, Generic, NFData)

type SimpleType = Int32 -- TODO: rename to a generic name; should not be related to a specific domain

data Condition
  = NodeTypeExists    Tag
  | SimpleTypeExists  SimpleType
  | AnyNotIn          (Set Tag)
  -- A field satisfies a predicate iff at least one of its possible values
  -- satisfy that predicate.
  -- NOTE: "non-deterministic" selector for Any?
  | Any               Predicate
  deriving (Eq, Ord, Show)

data Predicate
  = TagIn    (Set Tag)
  | TagNotIn (Set Tag)
  | ValueIn    Range
  | ValueNotIn Range
  deriving (Eq, Ord, Show)

-- inclusive lower, exclusive upper bound
data Range = Range { from :: Int32
                   , to   :: Int32
                   }
  deriving (Eq, Ord, Show)

-- TODO: error checking + validation ; DECISION: catch syntactical error at compile time ; the analyis will not be restrictive ; there will not be runtime checks

data Instruction
  = If
    { condition     :: Condition
    , srcReg        :: Reg
    , instructions  :: [Instruction]
    }
  -- | projects from the tag specific SRC's node part to DST reg simple type and location set
  | Project
    { srcSelector   :: Selector -- ^ the selected tag must exist
    , srcReg        :: Reg
    , dstReg        :: Reg
    }
  -- | extends DST's node part for a tag by SRC reg simple type and location set
  | Extend
    { srcReg      :: Reg
    , dstSelector :: Selector -- ^ the seleced tag must exist
    , dstReg      :: Reg
    }
  | Move
    { srcReg      :: Reg
    , dstReg      :: Reg
    }
  -- TODO: new, more general Move
  -- NOTE: same as Move, but only considers tags already present in dstReg
  --       (basically a Move but only for the common tags)
  | RestrictedMove
    { srcReg      :: Reg
    , dstReg      :: Reg
    }
  -- Moves all tags, but only those values that satisfy a given predicate.
  | ConditionalMove
    { srcReg      :: Reg
    , predicate   :: Predicate
    , dstReg      :: Reg
    }
  -- | copy mem (node) content addressed by SRC reg location part to DST register node part
  | Fetch
    { addressReg  :: Reg
    , dstReg      :: Reg
    }
  -- | copy the node part of the SRC reg to mem
  | Store
    { srcReg      :: Reg
    , address     :: Mem
    }
  -- | copy the node part of the SRC reg to mem addressed by DST reg location part
  | Update
    { srcReg      :: Reg
    , addressReg  :: Reg
    }
  -- NOTE: same as RestrictedMove, just for heap locations
  | RestrictedUpdate
    { srcReg      :: Reg
    , addressReg  :: Reg
    }
  -- Updates the heap with all tags, but only with those values that satisfy a given predicate.
  | ConditionalUpdate
    { srcReg      :: Reg
    , predicate   :: Predicate
    , addressReg  :: Reg
    }
  -- | copy compile time constant to DST register (one time setup)
  | Set
    { dstReg      :: Reg
    , constant    :: Constant
    }
  deriving (Eq, Ord, Show)

data Constant
  = CSimpleType   SimpleType
  | CHeapLocation Mem
  | CNodeType     Tag Int {-arity-}
  | CNodeItem     Tag Int {-node item index-} Int32 {-simple type, location, or incase of Cby: producer-}
  deriving (Eq, Ord, Show)

makeBaseFunctor ''Instruction

data AbstractProgram
  = AbstractProgram
  { _absMemoryCounter    :: Word32
  , _absRegisterCounter  :: Word32
  , _absInstructions     :: [Instruction]
  }
  deriving Show

data AbstractMapping
  = AbstractMapping
  { _absRegisterMap      :: Map.Map Name Reg
  , _absFunctionArgMap   :: Map.Map Name (Reg, [Reg])
  , _absTagMap           :: Bimap.Bimap Grin.Tag Tag
  }
  deriving Show

concat <$> mapM makeLenses [''AbstractProgram, ''AbstractMapping]

emptyAbstractProgram = AbstractProgram
  { _absMemoryCounter    = 0
  , _absRegisterCounter  = 0
  , _absInstructions     = []
  }

emptyAbstractMapping = AbstractMapping
  { _absRegisterMap      = Map.empty
  , _absFunctionArgMap   = Map.empty
  , _absTagMap           = Bimap.empty
  }
