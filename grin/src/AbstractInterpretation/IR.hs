{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.IR where

import Data.Int
import Data.Word

newtype Reg = Reg Word32 deriving (Eq, Ord)
newtype Mem = Mem Word32 deriving (Eq, Ord)

data Selector
  = All
  | SimpleTypes
  | Locations
  | Nodes
  | NodeItem Tag Int -- node item index
  -- TODO: con
{-
  case patterns - node with specific tag / specific simple type
  bind - all / node with specific tag
-}

newtype Tag = Tag Word32 deriving (Eq, Ord)
type SimpleType = Int32

data Condition
  = NodeTypeExists    Tag
  | SimpleTypeExists  SimpleType

-- TODO: error checking + validation ; DECISION: catch syntactical error on compile time ; the analyis will not be restrictive ; there will not be runtime checks

data Instruction
  = If
    { condition     :: Condition
    , srcReg        :: Reg
    , instructions  :: [Instruction]
    }
  | Move
    { valueSelector :: Selector
    , srcReg        :: Reg
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
  | Init -- ^ copy compile time constant to DST register (one time setup)
    { dstReg      :: Reg
    , constant    :: Constant
    } 

data Constant
  = CSimpleType   SimpleType
  | CHeapLocation Mem
  | CNodeType     Tag Int {-arity-}
  | CNodeItem     Tag Int {-node item index-} Int32 {-simple type or location-}
