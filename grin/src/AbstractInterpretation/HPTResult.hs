{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.HPTResult where

import Data.Set (Set)
import Data.Map (Map)
import Data.IntMap.Strict (IntMap)

import Grin

data CGType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Unit
  | T_Loc
  | T_Tag
  | T_UNKNOWN
  | T_Fun String
  deriving (Eq, Ord, Show)

data RTLocVal
  = RTLoc Int
  | BAS   CGType
  | RTVar Name -- HACK
  deriving (Eq, Ord, Show)

data RTNode = RTNode Tag [Set RTLocVal]
  deriving (Eq, Ord, Show)

data RTVar
  = N RTNode
  | V RTLocVal
  deriving (Eq, Ord, Show)

--type NodeSet = Set RTNode
type NodeSet = VarSet
type VarSet = Set RTVar -- HINT: VarVal in the paper

type HPTResult = Computer

data Computer
  = Computer
  { storeMap  :: IntMap NodeSet   -- models the computer memory
  , envMap    :: Map Name VarSet  -- models the CPU registers
  , steps     :: [Step]
  }
  deriving Show

data Step
  = StepExp     Exp
  | StepAssign  Name VarSet
  deriving Show

emptyComputer = Computer
  { storeMap  = mempty
  , envMap    = mempty
  , steps     = mempty
  }
