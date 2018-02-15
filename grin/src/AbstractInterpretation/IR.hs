{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.IR where

import Data.Int
import Data.Word

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map

import qualified Grin
import Grin (Name)

newtype Reg = Reg Word32 deriving (Eq, Ord, Show)
newtype Mem = Mem Word32 deriving (Eq, Ord, Show)

data Selector
  = NodeItem Tag Int -- node item index
  deriving Show

newtype Tag = Tag Word32 deriving (Eq, Ord, Show)
type SimpleType = Int32

data Condition
  = NodeTypeExists    Tag
  | SimpleTypeExists  SimpleType
  deriving Show

-- TODO: error checking + validation ; DECISION: catch syntactical error on compile time ; the analyis will not be restrictive ; there will not be runtime checks

data Instruction
  = If
    { condition     :: Condition
    , srcReg        :: Reg
    , instructions  :: [Instruction]
    }
  | Project -- ^ projects from the tag specific SRC's node part to DST reg simple type and location set
    { srcSelector   :: Selector -- ^ the seleced tag must exist
    , srcReg        :: Reg
    , dstReg        :: Reg
    }
  | Extend -- ^ extends DST's node part for a tag by SRC reg simple type and location set
    { srcReg        :: Reg
    , dstSelector   :: Selector -- ^ the seleced tag must exist
    , dstReg        :: Reg
    }
  | Move
    { srcReg        :: Reg
    , dstReg        :: Reg
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
  | Set -- ^ copy compile time constant to DST register (one time setup)
    { dstReg      :: Reg
    , constant    :: Constant
    }
  deriving Show

data Constant
  = CSimpleType   SimpleType
  | CHeapLocation Mem
  | CNodeType     Tag Int {-arity-}
  | CNodeItem     Tag Int {-node item index-} Int32 {-simple type or location-}
  deriving Show

data HPTProgram
  = HPTProgram
  { hptMemoryCounter    :: Word32
  , hptRegisterCounter  :: Word32
  , hptRegisterMap      :: Map.Map Name Reg
  , hptInstructions     :: [Instruction]
  , hptFunctionArgMap   :: Map.Map Name (Reg, [Reg])
  , hptTagMap           :: Bimap.Bimap Grin.Tag Tag
  }
  deriving Show

emptyHPTProgram = HPTProgram
  { hptMemoryCounter    = 0
  , hptRegisterCounter  = 0
  , hptRegisterMap      = Map.empty
  , hptInstructions     = []
  , hptFunctionArgMap   = Map.empty
  , hptTagMap           = Bimap.empty
  }
