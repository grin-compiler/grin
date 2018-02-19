{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Reducer.LLVM.Base where

import Debug.Trace
--import Text.Show.Pretty
import Text.Printf
import Control.Monad as M
import Control.Monad.State
import Data.Functor.Foldable as Foldable
import Lens.Micro.Platform

import Data.Map (Map)
import qualified Data.Map as Map

import Grin
import qualified TypeEnv

import LLVM.AST hiding (callingConvention)
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
data Env
  = Env
  { _envDefinitions   :: [Definition]
  , _envBasicBlocks   :: [BasicBlock]
  , _envInstructions  :: [Named Instruction]
  , _constantMap      :: Map Grin.Name Operand
  , _currentBlockName :: AST.Name
  , _envTempCounter   :: Int
  , _envTypeEnv       :: TypeEnv.TypeEnv
  , _envLLVMTypeMap   :: Map TypeEnv.NodeSet LLVM.Type
  , _envTagMap        :: Map Tag Constant
  }

emptyEnv = Env
  { _envDefinitions   = mempty
  , _envBasicBlocks   = mempty
  , _envInstructions  = mempty
  , _constantMap      = mempty
  , _currentBlockName = mkName ""
  , _envTempCounter   = 0
  , _envTypeEnv       = TypeEnv.TypeEnv mempty mempty mempty
  , _envLLVMTypeMap   = mempty
  , _envTagMap        = mempty
  }

concat <$> mapM makeLenses [''Env]

type CG = State Env

emit :: [Named Instruction] -> CG ()
emit instructions = modify' (\env@Env{..} -> env {_envInstructions = _envInstructions ++ instructions})

addConstant :: Grin.Name -> Operand -> CG ()
addConstant name operand = modify' (\env@Env{..} -> env {_constantMap = Map.insert name operand _constantMap})

unit :: CG Operand
unit = pure $ ConstantOperand $ Undef VoidType

undef :: Type -> Operand
undef = ConstantOperand . Undef

data Result
  = I Instruction
  | O Operand TypeEnv.Type

-- utils
closeBlock :: Terminator -> CG ()
closeBlock tr = modify' (\env@Env{..} -> env {_envInstructions = mempty, _envBasicBlocks = _envBasicBlocks ++ [BasicBlock _currentBlockName _envInstructions (Do tr)]})

startNewBlock :: AST.Name -> CG ()
startNewBlock name = modify' (\env@Env{..} -> env {_envInstructions = mempty, _currentBlockName = name})

addBlock :: AST.Name -> CG a -> CG a
addBlock name block = do
  instructions <- gets _envInstructions
  curBlockName <- gets _currentBlockName
  startNewBlock name
  result <- block
  modify' (\env@Env{..} -> env {_envInstructions = instructions, _currentBlockName = curBlockName})
  pure result

uniqueName :: String -> CG AST.Name
uniqueName name = state (\env@Env{..} -> (mkName $ printf "%s.%d" name _envTempCounter, env {_envTempCounter = succ _envTempCounter}))

getOperand :: Result -> CG Operand
getOperand = \case
  O a _ -> pure a
  I i -> do
          tmp <- uniqueName "tmp"
          emit [tmp := i]
          pure $ LocalReference i64 tmp -- TODO: handle type
