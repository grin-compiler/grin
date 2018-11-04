{-# LANGUAGE LambdaCase, RecordWildCards, RankNTypes #-}
module AbstractInterpretation.CodeGen where

import Data.Word
import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec

import Control.Monad.State
import Control.Monad.Trans.Except

import Grin.Grin
import Grin.TypeEnvDefs
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (AbstractProgram(..), HasDataFlowInfo(..))

type CG s a = ExceptT String (State s) a

data Result s
  = R IR.Reg
  | Z
  | A CPat (CG s (Result s))

emit :: HasDataFlowInfo s => IR.Instruction -> CG s ()
emit inst = modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {absInstructions = inst : absInstructions}

getsDfi :: (HasDataFlowInfo s, MonadState s m) => (AbstractProgram -> a) -> m a
getsDfi f = gets (f . getDataFlowInfo)

stateDfi :: (HasDataFlowInfo s, MonadState s m) =>
            (AbstractProgram -> (a, AbstractProgram)) -> m a
stateDfi f = do
  (res, dfi) <- getsDfi f
  modify $ modifyInfo $ const dfi
  return res

-- creates regsiters for function arguments and result
getOrAddFunRegs :: HasDataFlowInfo s => Name -> Int -> CG s (IR.Reg, [IR.Reg])
getOrAddFunRegs name arity = do
  funMap <- getsDfi absFunctionArgMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      resReg <- newReg
      argRegs <- replicateM arity newReg
      let funRegs = (resReg, argRegs)
      modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {absFunctionArgMap = Map.insert name funRegs absFunctionArgMap}
      pure funRegs

newReg :: HasDataFlowInfo s => CG s IR.Reg
newReg = stateDfi $ \s@AbstractProgram{..} -> (IR.Reg absRegisterCounter, s {absRegisterCounter = succ absRegisterCounter})

newMem :: HasDataFlowInfo s => CG s IR.Mem
newMem = stateDfi $ \s@AbstractProgram{..} -> (IR.Mem absMemoryCounter, s {absMemoryCounter = succ absMemoryCounter})

addReg :: HasDataFlowInfo s => Name -> IR.Reg -> CG s ()
addReg name reg = modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {absRegisterMap = Map.insert name reg absRegisterMap}

getReg :: HasDataFlowInfo s => Name -> CG s IR.Reg
getReg name = do
  regMap <- getsDfi absRegisterMap
  case Map.lookup name regMap of
    Nothing   -> throwE $ "unknown variable " ++ name
    Just reg  -> pure reg

getTag :: HasDataFlowInfo s => Tag -> CG s IR.Tag
getTag tag = do
  tagMap <- getsDfi absTagMap
  case Bimap.lookup tag tagMap of
    Just t  -> pure t
    Nothing -> do
      let t = IR.Tag . fromIntegral $ Bimap.size tagMap
      modify' $ modifyInfo $ \s -> s {absTagMap = Bimap.insert tag t tagMap}
      pure t

codeGenBlock :: HasDataFlowInfo s => CG s a -> CG s (a,[IR.Instruction])
codeGenBlock genM = do
  instructions <- stateDfi $ \s@AbstractProgram{..} -> (absInstructions, s {absInstructions = []})
  ret <- genM
  blockInstructions <- stateDfi $ \s@AbstractProgram{..} -> (reverse absInstructions, s {absInstructions = instructions})
  pure (ret, blockInstructions)

codeGenBlock_ :: HasDataFlowInfo s => CG s a -> CG s [IR.Instruction]
codeGenBlock_ = fmap snd . codeGenBlock

codeGenAlt :: HasDataFlowInfo s =>
              (Maybe Name, IR.Reg) ->
              (IR.Reg -> Name -> CG s IR.Reg) ->
              (IR.Reg -> CG s ()) ->
              CG s (Result s) ->
              (Result s -> CG s ()) ->
              (IR.Reg -> Name -> CG s ()) ->
              CG s [IR.Instruction]
codeGenAlt (mName, reg) restrict before altM after restore =
  codeGenBlock_ $ do
    altReg <- maybe (pure reg) (restrict reg) mName
    before altReg
    altResult <- altM
    after altResult
    maybe (pure ()) (restore reg) mName

emitMove :: HasDataFlowInfo s => IR.Reg -> IR.Reg -> CG s () 
emitMove src dst = emit IR.Move { srcReg = src, dstReg = dst }

emitExtendNodeItem :: HasDataFlowInfo s => 
                      IR.Reg -> IR.Tag -> Int -> IR.Reg -> CG s () 
emitExtendNodeItem src irTag idx dst = 
  emit IR.Extend { srcReg = src
                 , dstSelector = IR.NodeItem irTag idx
                 , dstReg = dst
                 }

-- TODO: rename simple type to something more generic,
newRegWithSimpleType :: HasDataFlowInfo s => IR.SimpleType -> CG s IR.Reg 
newRegWithSimpleType irTy = newReg >>= extendSimpleType irTy

-- TODO: rename simple type to something more generic,
extendSimpleType :: HasDataFlowInfo s => 
                    IR.SimpleType -> IR.Reg -> CG s IR.Reg 
extendSimpleType irTy r = do
  emit IR.Set
    { dstReg    = r
    , constant  = IR.CSimpleType irTy
    }
  pure r

codeGenSimpleType :: HasDataFlowInfo s => SimpleType -> CG s IR.Reg 
codeGenSimpleType = \case 
  T_Unit                -> newRegWithSimpleType (-1)
  T_Int64               -> newRegWithSimpleType (-2)
  T_Word64              -> newRegWithSimpleType (-3)
  T_Float               -> newRegWithSimpleType (-4)
  T_Bool                -> newRegWithSimpleType (-5)
  T_UnspecifiedLocation -> newRegWithSimpleType (-6)
  T_Location locs -> do
    r <- newReg
    let locs' = map fromIntegral locs
    mapM_ (`extendSimpleType` r) locs' 
    pure r
  t -> newReg
  

codeGenNodeSetWith :: HasDataFlowInfo s => 
                      (Tag -> Vector SimpleType -> CG s IR.Reg) -> 
                      NodeSet -> CG s IR.Reg 
codeGenNodeSetWith cgNodeTy ns = do 
  let (tags, argss) = unzip . Map.toList $ ns
  r <- newReg
  nodeRegs <- zipWithM cgNodeTy tags argss
  forM_ nodeRegs (`emitMove` r)
  pure r

codeGenType :: HasDataFlowInfo s => 
               (SimpleType -> CG s IR.Reg) ->
               (NodeSet -> CG s IR.Reg) ->
               Type -> CG s IR.Reg
codeGenType cgSimpleTy cgNodeTy = \case 
  T_SimpleType t -> cgSimpleTy t
  T_NodeSet   ns -> cgNodeTy ns