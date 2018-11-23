{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module AbstractInterpretation.IR where

import Data.Int
import Data.Word

import Data.Functor.Foldable.TH
import Control.DeepSeq
import GHC.Generics (Generic)

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import Data.Set (Set)

import Lens.Micro.Platform
import Lens.Micro.Internal

import qualified Grin.Grin as Grin
import Grin.Grin (Name)

newtype Reg = Reg Word32 deriving (Generic, NFData, Eq, Ord, Show)
newtype Mem = Mem Word32 deriving (Generic, NFData, Eq, Ord, Show)

data Selector
  = NodeItem              Tag Int   -- node item index
  | ConditionAsSelector   Condition
  | AllFields
  deriving (Generic, NFData, Eq, Ord, Show)

newtype Tag = Tag Word32 deriving (Generic, NFData, Eq, Ord, Show)

type SimpleType = Int32
type Producer   = Int32
type Liveness   = Int32

data Condition
  = NodeTypeExists    Tag
  | SimpleTypeExists  SimpleType
  | NotIn             (Set Tag)
  -- A field satisfies a predicate iff at least one of its possible values
  -- satisfy that predicate.
  -- NOTE: "non-deterministic" selector for Any?
  | Any               Predicate
  | All               Predicate
  deriving (Generic, NFData, Eq, Ord, Show)

data Predicate
  = TagIn    (Set Tag)
  | TagNotIn (Set Tag)
  | ValueIn    Range
  | ValueNotIn Range
  deriving (Generic, NFData, Eq, Ord, Show)

-- inclusive lower, exclusive upper bound
data Range = Range { from :: Int32
                   , to   :: Int32
                   }
  deriving (Generic, NFData, Eq, Ord, Show)

-- TODO: error checking + validation ; DECISION: catch syntactical error at compile time ; the analyis will not be restrictive ; there will not be runtime checks

data Instruction
  = If
    { condition     :: Condition
    , srcReg        :: Reg
    , instructions  :: [Instruction]
    }
  | Project -- ^ projects from the tag specific SRC's node part to DST reg simple type and location set
    { srcSelector   :: Selector -- ^ the selected tag must exist
    , srcReg        :: Reg
    , dstReg        :: Reg
    }
  | Extend -- ^ extends DST's node part for a tag by SRC reg simple type and location set
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
  | Fetch -- ^ copy mem (node) content addressed by SRC reg location part to DST register node part
    { addressReg  :: Reg
    , dstReg      :: Reg
    }
  | Store -- ^ copy the node part of the SRC reg to mem
    { srcReg      :: Reg
    , address     :: Mem
    }
  | Update -- ^ copy the node part of the SRC reg to mem addressed by DST reg location part
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
  | Set -- ^ copy compile time constant to DST register (one time setup)
    { dstReg      :: Reg
    , constant    :: Constant
    }
  deriving (Generic, NFData, Eq, Ord, Show)

data Constant
  = CSimpleType   SimpleType
  | CHeapLocation Mem
  | CNodeType     Tag Int {-arity-}
  | CNodeItem     Tag Int {-node item index-} Int32 {-simple type, location, or incase of Cby: producer-}
  deriving (Generic, NFData, Eq, Ord, Show)

makeBaseFunctor ''Instruction

class HasDataFlowInfo a where
  getDataFlowInfo :: a -> AbstractProgram
  modifyInfo :: (AbstractProgram -> AbstractProgram) -> a -> a


instance HasDataFlowInfo AbstractProgram where
  getDataFlowInfo = id
  modifyInfo f    = f

data AbstractProgram
  = AbstractProgram
  { _absMemoryCounter    :: Word32
  , _absRegisterCounter  :: Word32
  , _absRegisterMap      :: Map.Map Name Reg
  , _absInstructions     :: [Instruction]
  , _absFunctionArgMap   :: Map.Map Name (Reg, [Reg])
  , _absTagMap           :: Bimap.Bimap Grin.Tag Tag
  }
  deriving Show

concat <$> mapM makeLenses [''AbstractProgram]

emptyAbstractProgram = AbstractProgram
  { _absMemoryCounter    = 0
  , _absRegisterCounter  = 0
  , _absRegisterMap      = Map.empty
  , _absInstructions     = []
  , _absFunctionArgMap   = Map.empty
  , _absTagMap           = Bimap.empty
  }
