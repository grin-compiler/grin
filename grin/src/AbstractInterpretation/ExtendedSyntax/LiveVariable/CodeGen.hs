{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module AbstractInterpretation.ExtendedSyntax.LiveVariable.CodeGen
  ( module AbstractInterpretation.ExtendedSyntax.LiveVariable.CodeGen
  , live, sideEffecting
  ) where

import Control.Monad.State

import Data.Int

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Functor.Foldable as Foldable

import Lens.Micro.Platform

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty (PP(..))
import Grin.ExtendedSyntax.TypeEnvDefs
import Transformations.ExtendedSyntax.Util
import AbstractInterpretation.ExtendedSyntax.Util
import qualified AbstractInterpretation.ExtendedSyntax.IR as IR
import AbstractInterpretation.ExtendedSyntax.IR (Instruction(..), AbstractProgram(..), AbstractMapping(..))
import AbstractInterpretation.ExtendedSyntax.LiveVariable.CodeGenBase

import AbstractInterpretation.ExtendedSyntax.EffectTracking.Result

-- NOTE: For a live variable, we could store its type information.

-- Live variable analysis program.
-- For a basic value, a set containing "live" represents liveness (live is defined below).
-- The nth field of a node represents the liveness of the nth field.
-- By default, every variable is dead.
-- The data flows in two directions: the liveness information flows backwards,
-- the structural information flows forward.
type LVAMapping = AbstractMapping

type LivenessId = Int32

doNothing :: CG ()
doNothing = pure ()

emptyReg :: CG IR.Reg
emptyReg = newReg

-- Tests whether the given register is live.
isLiveThen :: IR.Reg -> [IR.Instruction] -> IR.Instruction
isLiveThen r is = IR.If { condition = IR.Any isLivenessInfo, srcReg = r, instructions = is }

isLiveThenM :: IR.Reg -> CG () -> CG ()
isLiveThenM r actionM = do
  is <- codeGenBlock_ actionM
  emit $ isLiveThen r is

setBasicValLiveInst :: IR.Reg -> IR.Instruction
setBasicValLiveInst r = IR.Set { dstReg = r, constant = IR.CSimpleType live }

setBasicValLive :: IR.Reg -> CG ()
setBasicValLive = emit . setBasicValLiveInst

setTagLive :: IR.Tag -> IR.Reg -> CG ()
setTagLive tag reg = do
  tmp <- newReg
  setBasicValLive tmp
  emit IR.Extend
    { srcReg      = tmp
    , dstSelector = IR.NodeItem tag 0
    , dstReg      = reg
    }

setAllTagsLive :: IR.Reg -> CG ()
setAllTagsLive reg = do
  tmp <- newReg
  setBasicValLive tmp
  emit IR.Extend
    { srcReg      = tmp
    , dstSelector = IR.EveryNthField 0
    , dstReg      = reg
    }

-- In order to Extend a node field, or Project into it, we need that field to exist.
-- This function initializes a node in the register with a given tag and arity.
setNodeTypeInfo :: IR.Reg -> IR.Tag -> Int -> Instruction
setNodeTypeInfo r t n = IR.Set { dstReg = r, constant = IR.CNodeType t n }

grinMain :: Name
grinMain = "grinMain"

setMainLive :: CG ()
setMainLive = do
  (mainRetReg, _) <- getOrAddFunRegs grinMain 0
  setLive mainRetReg

setLive :: IR.Reg -> CG ()
setLive r = do
  setBasicValLive r
  emit IR.Extend { srcReg = r, dstSelector = IR.AllFields, dstReg = r }

-- Only structural information should flow forwards,
-- and only liveness information should flow backwards.
{- Data flow info propagation for node pattern:
   case nodeReg of
     (CNode argReg) -> ...
   or
   (CNode argReg) <- pure nodeReg
-}
nodePatternDataFlow :: IR.Reg -> IR.Reg -> IR.Tag -> Int -> CG ()
nodePatternDataFlow argReg nodeReg irTag idx = do
  tmp    <- newReg

  -- propagating liveness info backwards
  emit IR.Extend { srcReg      = argReg
                 , dstSelector = IR.NodeItem irTag idx
                 , dstReg      = nodeReg
                 }

    -- propagating pointer info forwards
  emit IR.Project { srcReg      = nodeReg
                  , srcSelector = IR.NodeItem irTag idx
                  , dstReg      = tmp
                  }

  emit $ copyStructureWithPtrInfo tmp argReg

-- Only structural and effect information should flow forwards,
-- and only liveness information should flow backwards.
{- Data flow info propagation for variable patterns
   v <- pure ...
-}
varPatternDataFlow :: IR.Reg -> IR.Reg -> CG ()
varPatternDataFlow varReg lhsReg = do
  emit $ copyStructureWithPtrInfo lhsReg varReg
  effectDataFlow                  lhsReg varReg
  livenessDataFlow                varReg lhsReg

livenessDataFlow :: IR.Reg -> IR.Reg -> CG ()
livenessDataFlow srcReg dstReg = do
  tmp <- newReg
  emit $ copyStructureWithLivenessInfo srcReg tmp
  emit $ IR.RestrictedMove { srcReg = tmp, dstReg = dstReg }

effectDataFlow :: IR.Reg -> IR.Reg -> CG ()
effectDataFlow srcReg dstReg = do
  tmp <- newReg
  emit $ copyStructureWithEffectInfo srcReg tmp
  emit $ IR.RestrictedMove { srcReg = tmp, dstReg = dstReg }

-- Tests whether the given register has any side effects.
hasSideEffectsThen :: IR.Reg -> [IR.Instruction] -> IR.Instruction
hasSideEffectsThen r is = IR.If { condition = IR.Any isEffectInfo, srcReg = r, instructions = is }

hasSideEffectsThenM :: IR.Reg -> CG () -> CG ()
hasSideEffectsThenM r actionM = do
  is <- codeGenBlock_ actionM
  emit $ hasSideEffectsThen r is

setBasicValSideEffectingInst :: IR.Reg -> IR.Instruction
setBasicValSideEffectingInst r = IR.Set { dstReg = r, constant = IR.CSimpleType sideEffecting }

setBasicValSideEffecting :: IR.Reg -> CG ()
setBasicValSideEffecting = emit . setBasicValSideEffectingInst

codeGenVal :: Val -> CG IR.Reg
codeGenVal = \case
  ConstTagNode tag args -> do
    r <- newReg
    irTag <- getTag tag
    emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length args + 1)}
    forM_ (zip [1..] args) $ \(idx, arg) -> do
      tmp    <- newReg
      valReg <- getReg arg

      -- propagating liveness info backwards
      emit IR.Project { srcReg = r
                      , srcSelector = IR.NodeItem irTag idx
                      , dstReg = valReg
                      }

      -- propagating pointer info forwards
      emit $ copyStructureWithPtrInfo valReg tmp
      emit IR.Extend { srcReg      = tmp
                      , dstSelector = IR.NodeItem irTag idx
                      , dstReg      = r
                      }
    pure r
  Unit  -> emptyReg
  Lit _ -> emptyReg
  Var name -> getReg name
  Undefined t -> do
    r <- newReg
    typed <- codeGenType codeGenSimpleType (codeGenNodeSetWith codeGenTaggedNodeType) t
    emit $ copyStructureWithPtrInfo typed r
    pure r

codeGen :: Program -> (AbstractProgram, LVAMapping)
codeGen prg@(Program exts defs) = evalState (codeGenM prg >> mkAbstractProgramM) emptyCGState
codeGen _ = error "Program expected"

mkAbstractProgramM :: CG (AbstractProgram, LVAMapping)
mkAbstractProgramM = do
  CGState{..} <- get
  let prg = AbstractProgram
        { _absMemoryCounter   = _sMemoryCounter
        , _absRegisterCounter = _sRegisterCounter
        , _absInstructions    = _sInstructions
        }
  let mpg = AbstractMapping
        { _absRegisterMap     = _sRegisterMap
        , _absFunctionArgMap  = _sFunctionArgMap
        , _absTagMap          = _sTagMap
        }
  pure (prg, mpg)

codeGenM :: Exp -> CG ()
codeGenM e = (cata folder >=> const setMainLive) e
  where
  folder :: ExpF (CG Result) -> CG Result
  folder = \case
    ProgramF exts defs -> mapM_ addExternal exts >> sequence_ defs >> pure Z

    DefF f args body -> do
      (funResultReg, funArgRegs) <- getOrAddFunRegs f $ length args
      zipWithM_ addReg args funArgRegs
      body >>= \case
        Z   -> doNothing
        R r -> varPatternDataFlow funResultReg r
      pure Z

    EBindF leftExp bPat rightExp -> do
      lhs <- leftExp
      let R lhsReg = lhs

      let mkRegsThenVarPatternDataFlow v = do
            varReg <- newReg
            addReg v varReg
            varPatternDataFlow varReg lhsReg

      case bPat of
        VarPat v -> mkRegsThenVarPatternDataFlow v
        AsPat tag args v -> do
          irTag <- getTag tag
          setTagLive irTag lhsReg
          bindInstructions <- codeGenBlock_ $ forM (zip [1..] args) $ \(idx, arg) -> do
            argReg <- newReg
            addReg arg argReg
            nodePatternDataFlow argReg lhsReg irTag idx
          emit IR.If
            { condition     = IR.NodeTypeExists irTag
            , srcReg        = lhsReg
            , instructions  = bindInstructions
            }
          mkRegsThenVarPatternDataFlow v
        -- QUESTION: what about undefined?
        _ -> error $ "unsupported bpat " ++ show (PP bPat)

      rhs <- rightExp
      let R rhsReg = rhs
      effectDataFlow lhsReg rhsReg

      pure $ R rhsReg

    ECaseF scrut alts_ -> do
      originalScrutineeReg <- getReg scrut
      caseResultReg <- newReg

      alts <- sequence alts_

      let restrictExists tag scrutReg scrutName = do
            altScrutReg <- newReg
            addReg scrutName altScrutReg
            -- restricting scrutinee to alternative's domain
            emit IR.Project
              { srcSelector = IR.ConditionAsSelector $ IR.NodeTypeExists tag
              , srcReg = scrutReg
              , dstReg = altScrutReg
              }
            pure altScrutReg

          restrictNotIn tags scrutReg scrutName = do
            altScrutReg <- newReg
            addReg scrutName altScrutReg
            -- restricting scrutinee to alternative's domain
            emit IR.Project
              { srcSelector = IR.ConditionAsSelector $ IR.AnyNotIn tags
              , srcReg = scrutReg
              , dstReg = altScrutReg
              }
            pure altScrutReg

          -- caseResultReg is from global scope
          processAltResult = \case
            Z -> doNothing
            R altResultReg -> do
              --NOTE: We propagate liveness information from the case result register
              -- to the alt result register. But we also have to propagate
              -- structural and pointer information from the alt result register
              -- into the case result register.
              varPatternDataFlow caseResultReg altResultReg

          restoreScrutReg origScrutReg scrutName = do
            -- propagating info back to original scrutinee register
            altScrutReg <- getReg scrutName
            emit IR.Move
              { srcReg = altScrutReg
              , dstReg = origScrutReg
              }
            -- restoring scrut reg
            addReg scrutName origScrutReg

      forM_ alts $ \(A cpat altNameReg altM) -> do

        let codeGenAltExists tag before = codeGenAlt scrut
                                                     originalScrutineeReg
                                                     (restrictExists tag)
                                                     before
                                                     altM
                                                     processAltResult
                                                     restoreScrutReg

            codeGenAltNotIn tags before = codeGenAlt scrut
                                                     originalScrutineeReg
                                                     (restrictNotIn tags)
                                                     before
                                                     altM
                                                     processAltResult
                                                     restoreScrutReg

        {- NOTE: In case of a pattern match, all tags should be marked live.
           However, we allow for more aggressive optimizations if we only
           set a tag live, when it actually appears amongst the alternatives.
           This way we trust the program more than the analysis result, and
           all "possible" tags not appearing amongst the alernatives will
           stay dead.

           Also, if there is a #default alternative, we mark all tags live.
        -}
        case cpat of
          NodePat tag vars -> do
            irTag <- getTag tag
            altInstructions <- codeGenAltExists irTag $ \altScrutReg -> do
              -- NOTE: should be altResultRegister
              caseResultReg `isLiveThenM`         setTagLive irTag altScrutReg
              caseResultReg `hasSideEffectsThenM` setTagLive irTag altScrutReg
              varPatternDataFlow altNameReg altScrutReg
              -- bind pattern variables
              forM_ (zip [1..] vars) $ \(idx, name) -> do
                argReg <- newReg
                addReg name argReg
                nodePatternDataFlow argReg altScrutReg irTag idx
            emit IR.If
              { condition    = IR.NodeTypeExists irTag
              , srcReg       = originalScrutineeReg
              , instructions = altInstructions
              }

          -- NOTE: if we stored type information for basic val,
          -- we could generate code conditionally here as well
          LitPat lit -> do
            -- NOTE: should be altResultRegister
            caseResultReg `isLiveThenM`         setBasicValLive originalScrutineeReg
            caseResultReg `hasSideEffectsThenM` setBasicValLive originalScrutineeReg
            varPatternDataFlow altNameReg originalScrutineeReg
            altM >>= processAltResult

          DefaultPat -> do
            tags <- Set.fromList <$> sequence [getTag tag | A (NodePat tag _) _ _ <- alts]
            altInstructions <- codeGenAltNotIn tags $ \altScrutReg -> do
              caseResultReg `isLiveThenM`         (setBasicValLive altScrutReg >> setAllTagsLive altScrutReg)
              caseResultReg `hasSideEffectsThenM` (setBasicValLive altScrutReg >> setAllTagsLive altScrutReg)
              varPatternDataFlow altNameReg altScrutReg

            let canBeLiteral = null tags
            {- NOTE: Since, we are not tracking simple types (literals),
               the "AnyNotIn" condition will always be false for simple typed
               values. Hence, we need to check manually whether a scrutinee
               can be a simple typed value or not. If we never pattern match
               on a node tag, the scrutinee can be a simple typed value.

               Alternatively, we could track simple types, even with only
               some dummy values, but that would be unnecessary overhead.
            -}
            if canBeLiteral then
              mapM_ emit altInstructions
            else
              emit IR.If
                { condition    = IR.AnyNotIn tags
                , srcReg       = originalScrutineeReg
                , instructions = altInstructions
                }

      pure $ R caseResultReg

    AltF cpat n exp -> do
      altNameReg <- newReg
      addReg n altNameReg
      pure $ A cpat altNameReg exp

    SAppF name args -> do
      appReg  <- newReg
      argRegs <- mapM getReg args

      mExt <- getExternal name
      case mExt of
        Nothing -> do -- regular function
          (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
          -- no effect data-flow between formal and actual arguments
          zipWithM_ livenessDataFlow funArgRegs argRegs
          zipWithM_ (\src dst -> emit $ copyStructureWithPtrInfo src dst) argRegs funArgRegs

          varPatternDataFlow appReg funResultReg
        Just ext | eEffectful ext -> do mapM_ setBasicValLive argRegs
                                        setBasicValSideEffecting appReg
                 | otherwise      -> do allArgsLive <- codeGenBlock_ $ mapM_ setBasicValLive argRegs
                                        emit $ appReg `isLiveThen` allArgsLive

      pure $ R appReg

    SReturnF val -> R <$> codeGenVal val

    {- NOTE: Store is like an Update, just with a singleton address set
       (can only update a single heap location at a time).
       The other difference is that it also creates a new heap location.
       We will initialize this new heap location with structural information.
       Also, we only need information about tags already available
       in valReg, so we restrict the flow of information to those.
    -}
    SStoreF var -> do
      loc    <- newMem
      res    <- newReg
      tmp1   <- newReg
      tmp2   <- newReg
      varReg <- getReg var

      -- setting pointer information
      emit IR.Set { dstReg = res, constant = IR.CHeapLocation loc }

      -- copying structural information to the heap
      emit $ copyStructureWithPtrInfo varReg tmp1
      emit IR.Store          { srcReg = tmp1,   address = loc  }

      -- restrictively propagating info from heap
      emit IR.Fetch          { addressReg = res,  dstReg = tmp2   }
      emit IR.RestrictedMove { srcReg     = tmp2, dstReg = varReg }

      pure $ R res

    -- We want to update each location with only relevant information.
    -- This means, if a tag is not already present on that location,
    -- we do not update it.
    SFetchF ptr -> do
      ptrReg <- getReg ptr
      tmp    <- newReg
      res    <- newReg

      -- copying structural information from the heap
      emit IR.Fetch { addressReg = ptrReg, dstReg = tmp }
      emit $ copyStructureWithPtrInfo tmp res

      -- restrictively propagating info to heap
      emit IR.RestrictedUpdate {srcReg = res, addressReg = ptrReg}

      -- setting pointer liveness
      emit $ res `isLiveThen` [setBasicValLiveInst ptrReg]

      pure $ R res

    SUpdateF ptr var -> do
      ptrReg <- getReg ptr
      tmp1   <- newReg
      tmp2   <- newReg
      varReg <- getReg var

      -- copying structural information to the heap
      emit $ copyStructureWithPtrInfo varReg tmp1
      emit IR.Update         { srcReg = tmp1, addressReg = ptrReg  }

      -- restrictively propagating info from heap
      emit IR.Fetch          { addressReg = ptrReg, dstReg = tmp2   }
      emit IR.RestrictedMove { srcReg     = tmp2,   dstReg = varReg }

      -- setting pointer liveness
      emit $ varReg `isLiveThen` [setBasicValLiveInst ptrReg]

      R <$> newReg

    SBlockF exp -> exp

codeGenAlt :: Name ->
              IR.Reg ->
              (IR.Reg -> Name -> CG IR.Reg) ->
              (IR.Reg -> CG ()) ->
              CG Result ->
              (Result -> CG ()) ->
              (IR.Reg -> Name -> CG ()) ->
              CG [IR.Instruction]
codeGenAlt scrutName scrutReg restrict before altM after restore =
  codeGenBlock_ $ do
    altReg <- restrict scrutReg scrutName
    before altReg
    altResult <- altM
    after altResult
    restore scrutReg scrutName
