{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell #-}
module AbstractInterpretation.LiveVariable where

import Control.Monad.Trans.Except
import Control.Monad.State

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Functor.Foldable as Foldable

import Lens.Micro.Platform
import Lens.Micro.Internal

import Grin.Grin
import Transformations.Util
import AbstractInterpretation.Util
import AbstractInterpretation.CodeGen
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (Instruction(..), AbstractProgram(..), HasDataFlowInfo(..))

-- NOTE: For a live variable, we could store its type information.

-- Live variable analysis program.
-- For a basic value, a non-empty set represents liveness.
-- The nth field of a node represents the liveness of the nth field.
-- By default, every variable is dead (empty set ~ dead).
newtype LVAProgram = LVAProgram { _absProg :: AbstractProgram }
concat <$> mapM makeLenses [''LVAProgram]

instance HasDataFlowInfo LVAProgram where
  getDataFlowInfo = _absProg
  modifyInfo      = over absProg

emptyLVAProgram :: LVAProgram
emptyLVAProgram = LVAProgram IR.emptyAbstractProgram

type ResultLVA = Result LVAProgram

doNothing :: HasDataFlowInfo s => CG s ()
doNothing = pure ()

emptyReg :: HasDataFlowInfo s => CG s IR.Reg
emptyReg = newReg

-- Tests whether the give register is live.
isLiveThen :: IR.Reg -> [IR.Instruction] -> IR.Instruction
isLiveThen r i = IR.If { condition = IR.NotEmpty, srcReg = r, instructions = i }

live :: IR.Liveness
live = -1

setBasicValLive :: HasDataFlowInfo s => IR.Reg -> CG s ()
setBasicValLive r = emit IR.Set { dstReg = r, constant = IR.CSimpleType live }


-- In order to Extend a node field, or Project into it, we need that field to exist.
-- This function initializes a node in the register with a given tag and arity.
setNodeTypeInfo :: IR.Reg -> IR.Tag -> Int -> Instruction
setNodeTypeInfo r t n = IR.Set { dstReg = r, constant = IR.CNodeType t n }

grinMain :: Name
grinMain = "grinMain"

setMainLive :: HasDataFlowInfo s => CG s ()
setMainLive = do
  (mainRetReg, _) <- getOrAddFunRegs grinMain 0
  setBasicValLive mainRetReg
  setAllFieldsLive mainRetReg

setAllFieldsLive :: HasDataFlowInfo s => IR.Reg -> CG s ()
setAllFieldsLive r = do
  tmp <- newReg
  setBasicValLive tmp
  emit IR.Extend { srcReg = tmp, dstSelector = IR.AllFields, dstReg = r }

codeGenVal :: Val -> CG LVAProgram IR.Reg
codeGenVal = \case
  ConstTagNode tag vals -> do
    r <- newReg
    irTag <- getTag tag
    emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length vals)}
    forM_ (zip [0..] vals) $ \(idx, val) -> case val of
      Var name -> do
        valReg <- getReg name
        emit IR.Project {srcReg = r, srcSelector = IR.NodeItem irTag idx, dstReg = valReg}
      Lit lit -> doNothing
      _ -> throwE $ "illegal node item value " ++ show val
    pure r
  Unit  -> emptyReg
  Lit _ -> emptyReg
  Var name -> getReg name
  ValTag tag -> do
    r <- newReg
    irTag <- getTag tag
    emit IR.Set { dstReg = r, constant = IR.CNodeType irTag 1 }
    pure r
  val -> throwE $ "unsupported value " ++ show val


codeGen :: Exp -> Either String LVAProgram
codeGen = fmap reverseProgram
        . (\(a,s) -> s<$a)
        . flip runState emptyLVAProgram
        . runExceptT
        . (cata folder >=> const setMainLive)
  where
  folder :: ExpF (CG LVAProgram ResultLVA) -> CG LVAProgram ResultLVA
  folder = \case
    ProgramF defs -> sequence_ defs >> pure Z

    DefF name args body -> do
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      zipWithM_ addReg args funArgRegs
      body >>= \case
        Z   -> doNothing
        R r -> do emit IR.Move          { srcReg = funResultReg, dstReg = r }
                  emit IR.CopyStructure { srcReg = r, dstReg = funResultReg }
      -- emit $ funResultReg `isLiveThen` bodyInstructions
      pure Z

    EBindF leftExp lpat rightExp -> do
      leftExp >>= \case
        Z -> case lpat of
          Unit -> pure ()
          Var name -> do
            r <- newReg
            addReg name r
          _ -> throwE $ "pattern mismatch at HPT bind codegen, expected Unit got " ++ show lpat
        R r -> case lpat of
          Unit  -> setBasicValLive r
          Lit{} -> setBasicValLive r
          Var name -> addReg name r
          ConstTagNode tag args -> do
            irTag <- getTag tag
            -- emit $ setNodeTypeInfo r irTag (length args)
            bindInstructions <- codeGenBlock_ $ forM (zip [0..] args) $ \(idx, arg) ->
              case arg of
                Var name -> do
                  argReg <- newReg
                  addReg name argReg
                  emit IR.Extend { srcReg = argReg, dstSelector = IR.NodeItem irTag idx, dstReg = r }
                Lit {} -> emit IR.Set { dstReg = r, constant = IR.CNodeItem irTag idx live }
                _ -> throwE $ "illegal node pattern component " ++ show arg
            emit IR.If
              { condition     = IR.NodeTypeExists irTag
              , srcReg        = r
              , instructions  = bindInstructions
              }
          _ -> throwE $ "unsupported lpat " ++ show lpat
      rightExp

    ECaseF val alts_ -> do
      valReg <- codeGenVal val
      caseResultReg <- newReg

      -- save scrutinee register mapping
      scrutRegMapping <- case val of
        Var name -> Just . (name,) <$> getReg name
        _ -> pure Nothing
      alts <- sequence alts_

      forM_ alts $ \(A cpat altM) -> do
        -- performs a monadic action (probably binding variables in the CPat)
        -- then generates code for the Alt
        let codeGenAlt bindCPatVarsM = do
              bindInstructions <- codeGenBlock_ bindCPatVarsM
              altM >>= \case
                Z -> doNothing
                R altResultReg -> do
                  --NOTE: We propagateliveness information rom the case result register
                  -- to the alt result register. But we also have to propagate pointer
                  -- information from the alternative into the case result register.
                  -- Any information present in the alt result reg is always a
                  -- subset of the information present in the case result register.
                  -- This means, we do not propagate any additional information
                  -- with the second move instruction.
                  -- It is also needed for propagating node info into the
                  -- result register.
                  emit IR.RestrictedMove {srcReg = caseResultReg, dstReg = altResultReg}

                  -- this might probably could be replaced by a CopyStructure
                  -- and a "SimpleTypeCopy"
                  emit IR.Move {srcReg = altResultReg, dstReg = caseResultReg}
              return bindInstructions

        case cpat of
          NodePat tag vars -> do
            irTag <- getTag tag
            bindInstructions <- codeGenAlt $
              -- bind pattern variables
              forM_ (zip [0..] vars) $ \(idx, name) -> do
                argReg <- newReg
                addReg name argReg
                emit IR.Extend {srcReg = argReg, dstSelector = IR.NodeItem irTag idx, dstReg = valReg}
            emit IR.If
              { condition    = IR.NodeTypeExists irTag
              , srcReg       = valReg
              , instructions = bindInstructions
              }

          -- NOTE: if we stored type information for basic val,
          -- we could generate code conditionally here as well
          LitPat lit -> do
            bindInstructions <- codeGenAlt $ setBasicValLive valReg
            mapM_ emit bindInstructions

          -- We have no usable information.
          DefaultPat -> do
            bindInstructions <- codeGenAlt doNothing
            tags <- Set.fromList <$> sequence [getTag tag | A (NodePat tag _) _ <- alts]
            emit IR.If
              { condition    = IR.NotIn tags
              , srcReg       = valReg
              , instructions = bindInstructions
              }

          _ -> throwE $ "LVA does not support the following case pattern: " ++ show cpat

      -- restore scrutinee register mapping
      maybe (pure ()) (uncurry addReg) scrutRegMapping

      pure $ R caseResultReg

    AltF cpat exp -> pure $ A cpat exp

    SAppF name args -> do
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      valRegs <- mapM codeGenVal args
      zipWithM_ (\src dst -> emit IR.RestrictedMove {srcReg = src, dstReg = dst}) funArgRegs valRegs
      zipWithM_ (\src dst -> emit IR.CopyStructure {srcReg = src, dstReg = dst}) valRegs funArgRegs
      -- HINT: handle primop here because it does not have definition
      when (isPrimName name) $ codeGenPrimOp name funResultReg funArgRegs
      pure $ R funResultReg

    SReturnF val -> R <$> codeGenVal val

    -- Store is like an Update, just with a singleton address set
    -- (can only update a single heap location at a time).
    -- The other differnce is that it also creates a new heap location.
    -- We will initialize this new heap location with structural information.
    -- Also, we only need information about tags already available
    -- in valReg, so we restrict the flow of information to those.
    SStoreF val -> do
      loc    <- newMem
      r      <- newReg
      tmp1   <- newReg
      tmp2   <- newReg
      valReg <- codeGenVal val

      -- setting pointer information
      emit IR.Set           { dstReg = r, constant = IR.CHeapLocation loc }

      -- copying structural information to the heap
      emit IR.CopyStructure { srcReg = valReg, dstReg  = tmp1 }
      emit IR.Store         { srcReg = tmp1,   address = loc    }

      -- restrictively propagating info from heap
      emit IR.Fetch          { addressReg = r,    dstReg = tmp2   }
      emit IR.RestrictedMove { srcReg     = tmp2, dstReg = valReg }

      pure $ R r

    -- We want to update each location with only relevant information.
    -- This means, if a tag is not already present on that location,
    -- we do not update it.
    SFetchIF name maybeIndex -> case maybeIndex of
      Just {} -> throwE "LVA codegen does not support indexed fetch"
      Nothing -> do
        addressReg <- getReg name
        tmp        <- newReg
        r          <- newReg

        -- copying structural information from the heap
        emit IR.Fetch         { addressReg = addressReg, dstReg = tmp   }
        emit IR.CopyStructure { srcReg     = tmp,        dstReg     = r }

        -- restrictively propagating info from heap
        emit IR.RestrictedUpdate {srcReg = r, addressReg = addressReg}

        -- setting pointer liveness
        ptrIsLive <- codeGenBlock_ $ setBasicValLive addressReg
        emit $ r `isLiveThen` ptrIsLive

        pure $ R r

    SUpdateF name val -> do
      addressReg <- getReg name
      tmp        <- newReg
      valReg     <- codeGenVal val
      -- restrictively propagating info from heap
      emit IR.Fetch          {addressReg = addressReg, dstReg = tmp}
      emit IR.RestrictedMove {srcReg     = tmp,        dstReg = valReg}
      pure Z

    SBlockF exp -> exp

codeGenPrimOp :: HasDataFlowInfo s => Name -> IR.Reg -> [IR.Reg] -> CG s ()
codeGenPrimOp name funResultReg funArgRegs
  | name == "_prim_int_print" = mapM_ setBasicValLive funArgRegs
  | otherwise = do
    allArgsLive <- codeGenBlock_ $ mapM_ setBasicValLive funArgRegs
    emit $ funResultReg `isLiveThen` allArgsLive
