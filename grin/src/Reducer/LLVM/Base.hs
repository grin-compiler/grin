{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards, OverloadedStrings #-}

module Reducer.LLVM.Base where

import Debug.Trace
import Text.Show.Pretty
import Text.Printf
import Control.Monad as M
import Control.Monad.State
import Data.Functor.Foldable as Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Grin
import AbstractInterpretation.AbstractRunGrin (HPTResult(..), emptyComputer)

import LLVM.AST hiding (callingConvention)
import LLVM.AST.Type
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
  { envDefinitions    :: [Definition]
  , envBasicBlocks    :: [BasicBlock]
  , envInstructions   :: [Named Instruction]
  , constantMap       :: Map Grin.Name Operand
  , currentBlockName  :: AST.Name
  , envTempCounter    :: Int
  , envHPTResult      :: HPTResult
  }

emptyEnv = Env
  { envDefinitions    = mempty
  , envBasicBlocks    = mempty
  , envInstructions   = mempty
  , constantMap       = mempty
  , currentBlockName  = mkName ""
  , envTempCounter    = 0
  , envHPTResult      = emptyComputer
  }

type CG = State Env

emit :: [Named Instruction] -> CG ()
emit instructions = modify' (\env@Env{..} -> env {envInstructions = envInstructions ++ instructions})

addConstant :: Grin.Name -> Operand -> CG ()
addConstant name operand = modify' (\env@Env{..} -> env {constantMap = Map.insert name operand constantMap})

unit :: CG Operand
unit = pure $ ConstantOperand $ Undef VoidType

undef :: Type -> Operand
undef = ConstantOperand . Undef

data Result
  = I Instruction
  | O Operand

-- utils
closeBlock :: Terminator -> CG ()
closeBlock tr = modify' (\env@Env{..} -> env {envInstructions = mempty, envBasicBlocks = envBasicBlocks ++ [BasicBlock currentBlockName envInstructions (Do tr)]})

startNewBlock :: AST.Name -> CG ()
startNewBlock name = modify' (\env@Env{..} -> env {envInstructions = mempty, currentBlockName = name})

addBlock :: AST.Name -> CG a -> CG a
addBlock name block = do
  instructions <- gets envInstructions
  curBlockName <- gets currentBlockName
  startNewBlock name
  result <- block
  modify' (\env@Env{..} -> env {envInstructions = instructions, currentBlockName = curBlockName})
  pure result

uniqueTempName :: CG AST.Name
uniqueTempName = state (\env@Env{..} -> (mkName $ printf "tmp%d" envTempCounter, env {envTempCounter = succ envTempCounter}))

getOperand :: Result -> CG Operand
getOperand = \case
  O a -> pure a
  I i -> do
          tmp <- uniqueTempName
          emit [tmp := i]
          pure $ LocalReference i64 tmp -- TODO: handle type
