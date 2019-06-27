{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module AbstractInterpretation.LiveVariable.CodeGen
  ( module AbstractInterpretation.LiveVariable.CodeGen
  , live, sideEffecting
  ) where

import Control.Monad.State

import Data.Int

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Functor.Foldable as Foldable

import Lens.Micro.Platform

import Grin.Grin
import Grin.TypeEnvDefs
import Transformations.Util
import AbstractInterpretation.Util
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (Instruction(..), AbstractProgram(..), AbstractMapping(..))
import AbstractInterpretation.LiveVariable.CodeGenBase

import AbstractInterpretation.EffectTracking.Result

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
  ConstTagNode tag vals -> do
    r <- newReg
    irTag <- getTag tag
    emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length vals + 1)}
    forM_ (zip [1..] vals) $ \(idx, val) -> case val of
      Var name -> do
        tmp    <- newReg
        valReg <- getReg name

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
      Lit lit -> doNothing
      Undefined (T_SimpleType t) -> do
        -- undefined values should not have specified location info
        -- this is here "just in case"
        ptrInfo <- newReg
        tmp     <- codeGenSimpleType t
        emit $ copyStructureWithPtrInfo tmp ptrInfo
        emit IR.Extend
          { srcReg      = ptrInfo
          , dstSelector = IR.NodeItem irTag idx
          , dstReg      = r
          }
      _ -> error $ "illegal node item value " ++ show val
    pure r
  Unit  -> emptyReg
  Lit _ -> emptyReg
  Var name -> getReg name
  ValTag tag -> do
    r <- newReg
    irTag <- getTag tag
    emit IR.Set { dstReg = r, constant = IR.CNodeType irTag 1 }
    pure r
  Undefined t -> do
    r <- newReg
    typed <- codeGenType codeGenSimpleType (codeGenNodeSetWith codeGenTaggedNodeType) t
    emit $ copyStructureWithPtrInfo typed r
    pure r
  val -> error $ "unsupported value " ++ show val


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

    EBindF leftExp lpat rightExp -> do
      lhs <- leftExp
      let R lhsReg = lhs

      case lpat of
      {- NOTE: By convention, all bindings should have a variable pattern
          or a simple left-hand side of form `pure <var>`. This guarantees
          that all relevant computations will have a name. Also, it means
          the information has been already propagated to the variable.
      -}
        Unit  -> setBasicValLive lhsReg
        Lit{} -> setBasicValLive lhsReg
        Var v -> do
          varReg <- newReg
          addReg v varReg
          varPatternDataFlow varReg lhsReg
        ConstTagNode tag args -> do
          irTag <- getTag tag
          setTagLive irTag lhsReg
          bindInstructions <- codeGenBlock_ $ forM (zip [1..] args) $ \(idx, arg) ->
            case arg of
              Var name -> do
                argReg <- newReg
                addReg name argReg
                nodePatternDataFlow argReg lhsReg irTag idx
              Lit {} -> emit IR.Set { dstReg = lhsReg, constant = IR.CNodeItem irTag idx live }
              _ -> error $ "illegal node pattern component " ++ show arg
          emit IR.If
            { condition     = IR.NodeTypeExists irTag
            , srcReg        = lhsReg
            , instructions  = bindInstructions
            }
        _ -> error $ "unsupported lpat " ++ show lpat

      rhs <- rightExp
      let R rhsReg = rhs
      effectDataFlow lhsReg rhsReg

      pure $ R rhsReg

    ECaseF val alts_ -> do
      valReg <- codeGenVal val
      caseResultReg <- newReg

      -- save scrutinee register mapping
      scrutRegMapping <- case val of
        Var name -> pure (Just name, valReg)
        _        -> pure (Nothing,   valReg)

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

      forM_ alts $ \(A cpat altM) -> do

        let codeGenAltExists tag before = codeGenAlt scrutRegMapping
                                                     (restrictExists tag)
                                                     before
                                                     altM
                                                     processAltResult
                                                     restoreScrutReg

            codeGenAltNotIn tags before = codeGenAlt scrutRegMapping
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
              -- bind pattern variables
              forM_ (zip [1..] vars) $ \(idx, name) -> do
                argReg <- newReg
                addReg name argReg
                nodePatternDataFlow argReg altScrutReg irTag idx
            emit IR.If
              { condition    = IR.NodeTypeExists irTag
              , srcReg       = valReg
              , instructions = altInstructions
              }

          -- NOTE: if we stored type information for basic val,
          -- we could generate code conditionally here as well
          LitPat lit -> do
            -- NOTE: should be altResultRegister
            caseResultReg `isLiveThenM`         setBasicValLive valReg
            caseResultReg `hasSideEffectsThenM` setBasicValLive valReg
            altM >>= processAltResult

          DefaultPat -> do
            tags <- Set.fromList <$> sequence [getTag tag | A (NodePat tag _) _ <- alts]
            altInstructions <- codeGenAltNotIn tags $ \altScrutReg -> do
              caseResultReg `isLiveThenM`         (setBasicValLive altScrutReg >> setAllTagsLive altScrutReg)
              caseResultReg `hasSideEffectsThenM` (setBasicValLive altScrutReg >> setAllTagsLive altScrutReg)

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
                , srcReg       = valReg
                , instructions = altInstructions
                }

          _ -> error $ "LVA does not support the following case pattern: " ++ show cpat
      pure $ R caseResultReg

    AltF cpat exp -> pure $ A cpat exp

    SAppF name args -> do
      appReg  <- newReg
      argRegs <- mapM codeGenVal args

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
      emit IR.Set { dstReg = r, constant = IR.CHeapLocation loc }

      -- copying structural information to the heap
      emit $ copyStructureWithPtrInfo valReg tmp1
      emit IR.Store          { srcReg = tmp1,   address = loc  }

      -- restrictively propagating info from heap
      emit IR.Fetch          { addressReg = r,    dstReg = tmp2   }
      emit IR.RestrictedMove { srcReg     = tmp2, dstReg = valReg }

      pure $ R r

    -- We want to update each location with only relevant information.
    -- This means, if a tag is not already present on that location,
    -- we do not update it.
    SFetchIF name maybeIndex -> case maybeIndex of
      Just {} -> error "LVA codegen does not support indexed fetch"
      Nothing -> do
        addressReg <- getReg name
        tmp        <- newReg
        r          <- newReg

        -- copying structural information from the heap
        emit IR.Fetch { addressReg = addressReg, dstReg = tmp }
        emit $ copyStructureWithPtrInfo tmp r

        -- restrictively propagating info to heap
        emit IR.RestrictedUpdate {srcReg = r, addressReg = addressReg}

        -- setting pointer liveness
        emit $ r `isLiveThen` [setBasicValLiveInst addressReg]

        pure $ R r

    SUpdateF name val -> do
      addressReg <- getReg name
      tmp1       <- newReg
      tmp2       <- newReg
      valReg     <- codeGenVal val

      -- copying structural information to the heap
      emit $ copyStructureWithPtrInfo valReg tmp1
      emit IR.Update         { srcReg = tmp1, addressReg = addressReg  }

      -- restrictively propagating info from heap
      emit IR.Fetch          { addressReg = addressReg, dstReg = tmp2   }
      emit IR.RestrictedMove { srcReg     = tmp2,       dstReg = valReg }

      -- setting pointer liveness
      emit $ valReg `isLiveThen` [setBasicValLiveInst addressReg]

      R <$> newReg

    SBlockF exp -> exp

codeGenAlt :: (Maybe Name, IR.Reg) ->
              (IR.Reg -> Name -> CG IR.Reg) ->
              (IR.Reg -> CG ()) ->
              CG Result ->
              (Result -> CG ()) ->
              (IR.Reg -> Name -> CG ()) ->
              CG [IR.Instruction]
codeGenAlt (mName, reg) restrict before altM after restore =
  codeGenBlock_ $ do
    altReg <- maybe (pure reg) (restrict reg) mName
    before altReg
    altResult <- altM
    after altResult
    mapM_ (restore reg) mName
