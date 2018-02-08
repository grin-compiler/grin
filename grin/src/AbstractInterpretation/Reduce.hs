{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.Reduce where

import Data.Int
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.State
import Control.Monad.Reader

import AbstractInterpretation.IR
import AbstractInterpretation.CodeGen

type NodeSet = Map Tag (Vector (Set Int32))

data Value
  = Value
  { simpleTypeAndLocationSet  :: Set Int32
  , nodeSet                   :: NodeSet
  }
  deriving Eq

data Computer
  = Computer
  { memory    :: Vector NodeSet
  , register  :: Vector Value
  }

type HPT = ReaderT HPTProgram (State Computer)

unionNodeSet :: NodeSet -> NodeSet -> NodeSet
unionNodeSet = Map.unionWith unionNodeData where
  unionNodeData a b
    | V.length a == V.length b = V.zipWith Set.union a b
    | otherwise = error $ "node arity mismatch " ++ show (V.length a) ++ " =/= " ++ show (V.length b)

unionValue :: Value -> Value -> Value
unionValue a b = Value
  { simpleTypeAndLocationSet  = Set.union (simpleTypeAndLocationSet a) (simpleTypeAndLocationSet b)
  , nodeSet                   = unionNodeSet (nodeSet a) (nodeSet b)
  }

evalInstruction :: Instruction -> HPT ()
evalInstruction = \case
  {-
  If      {..} -> keyword "if" <+> pretty condition <+> keyword "in" <+> pretty srcReg <$$> indent 2 (pretty instructions)
  Project {..} -> keyword "project" <+> pretty srcSelector <+> pretty srcReg <+> pretty dstReg
  Extend  {..} -> keyword "extend" <+> pretty srcReg <+> pretty dstSelector <+> pretty dstReg
  -}
  --Move    {..} -> keyword "move" <+> pretty srcReg <+> pretty dstReg
  {-
  Fetch   {..} -> keyword "fetch" <+> pretty addressReg <+> pretty dstReg
  Store   {..} -> keyword "store" <+> pretty srcReg <+> pretty address
  Update  {..} -> keyword "update" <+> pretty srcReg <+> pretty addressReg
  Init    {..} -> keyword "init" <+> pretty dstReg <+> pretty constant
  -}
  _ -> pure ()
{-
  If
  { condition     :: Condition
  , srcReg        :: Reg
  , instructions  :: [Instruction]
  }
  Project
  { srcSelector   :: Selector
  , srcReg        :: Reg
  , dstReg        :: Reg
  }
  Extend
  { srcReg        :: Reg
  , dstSelector   :: Selector
  , dstReg        :: Reg
  }
  Move
  { srcReg        :: Reg
  , dstReg        :: Reg
  }
  Fetch -- ^ copy mem (node) content addressed by SRC reg location part to DST register node part
  { addressReg  :: Reg
  , dstReg      :: Reg
  }
  Store -- ^ copy the node part of the SRC reg to mem
  { srcReg      :: Reg
  , address     :: Mem
  }
  Update -- ^ copy the node part of the SRC reg to mem addressed by DST reg location part
  { srcReg      :: Reg
  , addressReg  :: Reg
  }
  Init -- ^ copy compile time constant to DST register (one time setup)
  { dstReg      :: Reg
  , constant    :: Constant
  }

-}
