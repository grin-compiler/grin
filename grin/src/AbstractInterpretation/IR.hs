{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.IR where

import Data.Int
import Data.Word

type DSTreg = Word32
type SRCreg = Word32
type DSTmem = Word32
type SRCmem = Word32

data Selector
  = All
  | TagsOnly
  | SimpleTypesOnly
  | LocationsOnly
  | NodesOnly
{-
  case patterns - node with specific tag / specific simple type / specific tag / 
  bind - all / any node / node with specific tag
-}

data Condition
  = EQTag         Word32
  | EQSimpleType  Int32

-- TODO: error checking + validation ; DECISION: catch syntactical error on compile time ; the analyis will not be restrictive ; there will not be runtime checks

data Instruction
  = If        Condition [Instruction]
  | Move      SRCreg DSTreg Selector
  | Load      SRCmem DSTreg -- copy mem (node) content to DST register node part
  | Store     SRCreg DSTmem -- copy the node part of the SRC reg to mem
  | LoadLoc   SRCreg DSTreg -- copy mem (node) content addressed by SRC reg location part to DST register node part
  | StoreLoc  SRCreg DSTreg -- copy the node part of the SRC reg to mem addressed by DST reg location part
