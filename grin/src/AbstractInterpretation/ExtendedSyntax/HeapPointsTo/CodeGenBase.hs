{-# LANGUAGE LambdaCase, RecordWildCards, RankNTypes, TemplateHaskell #-}
module AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGenBase where

import Data.Int
import Data.Word
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec

import Control.Monad.State

import Grin.ExtendedSyntax.Grin (Name, SimpleType(..), CPat(..), unpackName, Tag(..), External(..))
import Grin.ExtendedSyntax.TypeEnvDefs
import AbstractInterpretation.ExtendedSyntax.IR (Instruction(..), Reg(..), AbstractMapping)
import qualified AbstractInterpretation.ExtendedSyntax.IR as IR

import Lens.Micro.Platform

type HPTMapping = AbstractMapping -- TODO

data CGState
  = CGState
  { _sMemoryCounter   :: Word32
  , _sRegisterCounter :: Word32
  , _sInstructions    :: [Instruction]

  -- mapping

  , _sRegisterMap     :: Map Name Reg
  , _sFunctionArgMap  :: Map Name (Reg, [Reg])
  , _sTagMap          :: Bimap.Bimap Tag IR.Tag

  -- internal

  , _sExternalMap     :: Map Name External
  }
  deriving (Show)

concat <$> mapM makeLenses [''CGState]

emptyCGState :: CGState
emptyCGState = CGState
  { _sMemoryCounter   = 0
  , _sRegisterCounter = 0
  , _sInstructions    = []

  -- mapping

  , _sRegisterMap     = mempty
  , _sFunctionArgMap  = mempty
  , _sTagMap          = Bimap.empty

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
addExternal e = modify' $ \s@CGState{..} -> s {_sExternalMap = Map.insert (eName e) e _sExternalMap}

getExternal :: Name -> CG (Maybe External)
getExternal name = Map.lookup name <$> gets _sExternalMap

-- creates regsiters for function arguments and result
getOrAddFunRegs :: Name -> Int -> CG (IR.Reg, [IR.Reg])
getOrAddFunRegs name arity = do
  funMap <- gets _sFunctionArgMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      resReg <- newReg
      argRegs <- replicateM arity newReg
      let funRegs = (resReg, argRegs)
      modify' $ \s@CGState{..} -> s {_sFunctionArgMap = Map.insert name funRegs _sFunctionArgMap}
      pure funRegs

newReg :: CG IR.Reg
newReg = state $ \s@CGState{..} -> (IR.Reg _sRegisterCounter, s {_sRegisterCounter = succ _sRegisterCounter})

newMem :: CG IR.Mem
newMem = state $ \s@CGState{..} -> (IR.Mem _sMemoryCounter, s {_sMemoryCounter = succ _sMemoryCounter})

addReg :: Name -> IR.Reg -> CG ()
addReg name reg = modify' $ \s@CGState{..} -> s {_sRegisterMap = Map.insert name reg _sRegisterMap}

getReg :: Name -> CG IR.Reg
getReg name = do
  regMap <- gets _sRegisterMap
  case Map.lookup name regMap of
    Nothing   -> error $ "unknown variable " ++ unpackName name
    Just reg  -> pure reg

getTag :: Tag -> CG IR.Tag
getTag tag = do
  tagMap <- gets _sTagMap
  case Bimap.lookup tag tagMap of
    Just t  -> pure t
    Nothing -> do
      let t = IR.Tag . fromIntegral $ Bimap.size tagMap
      modify' $ \s -> s {_sTagMap = Bimap.insert tag t tagMap}
      pure t

codeGenBlock :: CG a -> CG (a,[IR.Instruction])
codeGenBlock genM = do
  instructions <- state $ \s@CGState{..} -> (_sInstructions, s {_sInstructions = []})
  ret <- genM
  blockInstructions <- state $ \s@CGState{..} -> (reverse _sInstructions, s {_sInstructions = instructions})
  pure (ret, blockInstructions)

codeGenBlock_ :: CG a -> CG [IR.Instruction]
codeGenBlock_ = fmap snd . codeGenBlock

codeGenSimpleType :: SimpleType -> CG IR.Reg
codeGenSimpleType = \case
  T_Unit                -> newRegWithSimpleType (-1)
  T_Int64               -> newRegWithSimpleType (-2)
  T_Word64              -> newRegWithSimpleType (-3)
  T_Float               -> newRegWithSimpleType (-4)
  T_Bool                -> newRegWithSimpleType (-5)
  T_String              -> newRegWithSimpleType (-6)
  T_Char                -> newRegWithSimpleType (-7)
  T_UnspecifiedLocation -> newRegWithSimpleType (-8)
  T_Location locs -> do
    r <- newReg
    let locs' = map fromIntegral locs
    mapM_ (`extendSimpleType` r) locs'
    pure r
  t -> newReg
  where
  -- TODO: rename simple type to something more generic,
  newRegWithSimpleType :: IR.SimpleType -> CG IR.Reg
  newRegWithSimpleType irTy = newReg >>= extendSimpleType irTy

  -- TODO: rename simple type to something more generic,
  extendSimpleType :: IR.SimpleType -> IR.Reg -> CG IR.Reg
  extendSimpleType irTy r = do
    emit IR.Set
      { dstReg    = r
      , constant  = IR.CSimpleType irTy
      }
    pure r

codeGenNodeSetWith :: (Tag -> Vector SimpleType -> CG IR.Reg) -> NodeSet -> CG IR.Reg
codeGenNodeSetWith cgNodeTy ns = do
  let (tags, argss) = unzip . Map.toList $ ns
  dst <- newReg
  nodeRegs <- zipWithM cgNodeTy tags argss
  forM_ nodeRegs $ \src -> emit IR.Move { srcReg = src, dstReg = dst }
  pure dst

-- Generate a node type from type information,
-- but preserve the first field for tag information.
codeGenTaggedNodeType :: Tag -> Vector SimpleType -> CG IR.Reg -- delete
codeGenTaggedNodeType tag ts = do
  let ts' = Vec.toList ts
  r <- newReg
  irTag <- getTag tag
  argRegs <- mapM codeGenSimpleType ts'
  emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length argRegs + 1)}
  forM_ (zip [1..] argRegs) $ \(idx, argReg) ->
    emit IR.Extend {srcReg = argReg, dstSelector = IR.NodeItem irTag idx, dstReg = r}
  pure r

-- FIXME: the following type signature is a bad oman ; it's not intuitive ; no-go ; refactor!
codeGenType :: (SimpleType -> CG IR.Reg) -> (NodeSet -> CG IR.Reg) -> Type -> CG IR.Reg
codeGenType cgSimpleTy cgNodeTy = \case
  T_SimpleType t -> cgSimpleTy t
  T_NodeSet   ns -> cgNodeTy ns

isPointer :: IR.Predicate
isPointer = IR.ValueIn (IR.Range 0 (maxBound :: Int32))

isNotPointer :: IR.Predicate -- delete
isNotPointer = IR.ValueIn (IR.Range (minBound :: Int32) 0)

-- For simple types, copies only pointer information
-- For nodes, copies the structure and the pointer information in the fields
copyStructureWithPtrInfo :: IR.Reg -> IR.Reg -> IR.Instruction
copyStructureWithPtrInfo srcReg dstReg = IR.ConditionalMove
  { srcReg    = srcReg
  , predicate = isPointer
  , dstReg    = dstReg
  }
