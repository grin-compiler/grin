{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Reducer.LLVM.Base where

import Text.Printf
import Control.Monad as M
import Control.Monad.State
import Data.Functor.Foldable as Foldable
import Lens.Micro.Platform

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)

import Grin
import qualified TypeEnv

import LLVM.AST as LLVM hiding (callingConvention)
import LLVM.AST.Type as LLVM
import LLVM.AST.AddrSpace
import LLVM.AST.Constant hiding (Add, ICmp)
import LLVM.AST.IntegerPredicate
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

heapPointerName :: String
heapPointerName = "_heap_ptr_"

tagLLVMType :: LLVM.Type
tagLLVMType = i64

locationLLVMType :: LLVM.Type
locationLLVMType = ptr tagLLVMType

data Env
  = Env
  { _envDefinitions       :: [Definition]                     -- Program state
  , _envBasicBlocks       :: Map Int BasicBlock               -- Def state ;  order -> basic block
  , _envInstructions      :: [Named Instruction]              -- Def state
  , _constantMap          :: Map Grin.Name Operand            -- Def state
  , _currentBlockName     :: AST.Name                         -- Def state
  , _envBlockInstructions :: Map AST.Name [Named Instruction] -- Def state
  , _envBlockOrder        :: Map AST.Name Int                 -- Def state
  , _envTempCounter       :: Int
  , _envTypeEnv           :: TypeEnv.TypeEnv
  , _envTagMap            :: Map Tag Constant
  }

emptyEnv = Env
  { _envDefinitions       = mempty
  , _envBasicBlocks       = mempty
  , _envInstructions      = mempty
  , _constantMap          = mempty
  , _currentBlockName     = mkName ""
  , _envBlockInstructions = mempty
  , _envBlockOrder        = mempty
  , _envTempCounter       = 0
  , _envTypeEnv           = TypeEnv.TypeEnv mempty mempty mempty
  , _envTagMap            = mempty
  }

concat <$> mapM makeLenses [''Env]

-- Tagged union
{-
  HINT: tagged union LLVM representation

    struct {
      Int64 tag;
      Int64[N1];
      Word64[N2];
      ...
    }
-}
data TUIndex
  = TUIndex
  { tuStructIndex   :: Word32
  , tuArrayIndex    :: Word32
  , tuItemLLVMType  :: LLVM.Type
  }
  deriving (Eq, Ord, Show)

data TaggedUnion
  = TaggedUnion
  { tuLLVMType  :: LLVM.Type -- struct of arrays of SimpleType with size
  , tuMapping   :: Map Tag (Vector TUIndex)
  }
  deriving (Eq, Ord, Show)

data CGType
  = CG_SimpleType
    { cgLLVMType    :: LLVM.Type
    , cgType        :: TypeEnv.Type
    }
  | CG_NodeSet
    { cgLLVMType    :: LLVM.Type
    , cgType        :: TypeEnv.Type
    , cgTaggedUnion :: TaggedUnion
    }
  deriving (Eq, Ord, Show)

type CG = State Env

emit :: [Named Instruction] -> CG ()
emit instructions = modify' (\env@Env{..} -> env {_envInstructions = _envInstructions ++ instructions})

addConstant :: Grin.Name -> Operand -> CG ()
addConstant name operand = modify' (\env@Env{..} -> env {_constantMap = Map.insert name operand _constantMap})

unit :: Operand
unit = ConstantOperand $ Undef VoidType

undef :: Type -> Operand
undef = ConstantOperand . Undef

data Result
  = I CGType Instruction
  | O CGType Operand

-- utils
closeBlock :: Terminator -> CG ()
closeBlock tr = modify' $ \env@Env{..} -> env
  { _envInstructions      = mempty
  , _envBasicBlocks       = Map.insert (Map.findWithDefault undefined _currentBlockName _envBlockOrder) (BasicBlock _currentBlockName _envInstructions (Do tr)) _envBasicBlocks
  , _envBlockInstructions = Map.delete _currentBlockName _envBlockInstructions
  , _currentBlockName     = mkName ""
  }

activeBlock :: AST.Name -> CG ()
activeBlock name = modify' f where
  f env@Env{..}
    | name == _currentBlockName = env
    | otherwise = env
      { _envInstructions      = Map.findWithDefault mempty name _envBlockInstructions
      , _currentBlockName     = name
      , _envBlockInstructions = Map.insert _currentBlockName _envInstructions _envBlockInstructions
      , _envBlockOrder        = Map.insert name (Map.findWithDefault (Map.size _envBlockOrder) name _envBlockOrder) _envBlockOrder
      }

uniqueName :: String -> CG AST.Name
uniqueName name = state (\env@Env{..} -> (mkName $ printf "%s.%d" name _envTempCounter, env {_envTempCounter = succ _envTempCounter}))

getOperand :: String -> Result -> CG (CGType, Operand)
getOperand name = \case
  O cgTy a -> pure (cgTy, a)
  I cgTy i -> case cgLLVMType cgTy of
    VoidType  -> emit [Do i] >> pure (cgTy, unit)
    t         -> (cgTy,) <$> codeGenLocalVar name t i

codeGenLocalVar :: String -> LLVM.Type -> AST.Instruction -> CG LLVM.Operand
codeGenLocalVar name ty instruction = do
  varName <- uniqueName name
  emit [varName := instruction]
  pure $ LocalReference ty varName
