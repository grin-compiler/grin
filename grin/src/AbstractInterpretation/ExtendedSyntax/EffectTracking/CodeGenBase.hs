{-# LANGUAGE LambdaCase, RecordWildCards, RankNTypes, TemplateHaskell #-}
module AbstractInterpretation.ExtendedSyntax.EffectTracking.CodeGenBase where

import Data.Int
import Data.Word
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec

import Data.Functor.Infix

import Control.Monad.State

import Grin.ExtendedSyntax.Grin (Name, SimpleType(..), CPat(..), unpackName, Tag(..), External(..))
import Grin.ExtendedSyntax.TypeEnvDefs
import AbstractInterpretation.ExtendedSyntax.IR (Instruction(..), Reg(..), AbstractMapping)
import qualified AbstractInterpretation.ExtendedSyntax.IR as IR

import Lens.Micro.Platform

type ExternalID = Int32

data ExternalWithID = E { extID  :: ExternalID
                        , extExt :: External
                        }
  deriving (Eq, Ord, Show)

data ETMapping
  = ETMapping
  { _etRegisterMap      :: Map Name Reg
  , _etFunctionRetMap   :: Map Name Reg
  , _etExternalMap      :: Map ExternalID External
  }
  deriving Show

data CGState
  = CGState
  { _sRegisterCounter :: Word32
  , _sInstructions    :: [Instruction]

  -- mapping

  , _sRegisterMap     :: Map Name Reg
  , _sFunctionRetMap  :: Map Name Reg

  -- internal

  , _sExternalMap     :: Map Name ExternalWithID
  }
  deriving (Show)

concat <$> mapM makeLenses [''CGState]

emptyCGState :: CGState
emptyCGState = CGState
  { _sRegisterCounter = 0
  , _sInstructions    = []

  -- mapping

  , _sRegisterMap     = mempty
  , _sFunctionRetMap  = mempty

  -- internal

  , _sExternalMap     = mempty
  }

type CG = State CGState

data Result
  = R IR.Reg
  | Z
  | A CPat IR.Reg (CG Result)

emit :: IR.Instruction -> CG ()
emit inst = modify' $ \s@CGState{..} -> s {_sInstructions = inst : _sInstructions}

addExternal :: External -> CG ()
addExternal e = do
  let name = eName e
  eMap <- gets _sExternalMap
  if name `Map.member` eMap then
    error $ "External already present in the external map: " ++ show name
  else
    modify' $ \s@CGState{..} ->
    let curSize = fromIntegral . Map.size $ _sExternalMap in
    s { _sExternalMap = Map.insert (eName e) (E curSize e) _sExternalMap
      , _sFunctionRetMap = mempty
      }

getExternal :: Name -> CG (Maybe External)
getExternal name = extExt <$$> Map.lookup name <$> gets _sExternalMap

getExternalID :: Name -> CG (Maybe ExternalID)
getExternalID name = extID <$$> Map.lookup name <$> gets _sExternalMap

-- creates regsiters for function arguments and result
getOrAddFunRetReg :: Name -> CG IR.Reg
getOrAddFunRetReg name = do
  funMap <- gets _sFunctionRetMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      retReg <- newReg
      modify' $ \s@CGState{..} -> s {_sFunctionRetMap = Map.insert name retReg _sFunctionRetMap}
      pure retReg

newReg :: CG IR.Reg
newReg = state $ \s@CGState{..} -> (IR.Reg _sRegisterCounter, s {_sRegisterCounter = succ _sRegisterCounter})

addReg :: Name -> IR.Reg -> CG ()
addReg name reg = modify' $ \s@CGState{..} -> s {_sRegisterMap = Map.insert name reg _sRegisterMap}

getReg :: Name -> CG IR.Reg
getReg name = do
  regMap <- gets _sRegisterMap
  case Map.lookup name regMap of
    Nothing   -> error $ "unknown variable " ++ unpackName name
    Just reg  -> pure reg

codeGenBlock :: CG a -> CG (a,[IR.Instruction])
codeGenBlock genM = do
  instructions <- state $ \s@CGState{..} -> (_sInstructions, s {_sInstructions = []})
  ret <- genM
  blockInstructions <- state $ \s@CGState{..} -> (reverse _sInstructions, s {_sInstructions = instructions})
  pure (ret, blockInstructions)

codeGenBlock_ :: CG a -> CG [IR.Instruction]
codeGenBlock_ = fmap snd . codeGenBlock

