{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module AbstractInterpretation.ExtendedSyntax.CreatedBy.CodeGen where

import Control.Monad.Writer
import Control.Monad.State

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Data.Functor.Foldable as Foldable

import Lens.Micro.Platform

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty (PP(..))
import Grin.ExtendedSyntax.TypeEnvDefs
import qualified AbstractInterpretation.ExtendedSyntax.IR as IR
import AbstractInterpretation.ExtendedSyntax.IR (Instruction(..), AbstractProgram(..), emptyAbstractProgram, AbstractMapping(..))
import AbstractInterpretation.ExtendedSyntax.CreatedBy.CodeGenBase
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGen (litToSimpleType, unitType, codegenSimpleType) -- FIXME: why? remove, refactor
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result (undefinedProducer) -- FIXME: why? remove, refactor


data CByMapping
  = CByMapping
  { _producerMap  :: Map.Map IR.Reg Name
  , _hptMapping   :: AbstractMapping
  } deriving (Show)

concat <$> mapM makeLenses [''CByMapping]

-- HPT program with producer information about nodes ; for each node, it contains the node's possible producers in the first field
mkCByProgramM :: CG (AbstractProgram, CByMapping)
mkCByProgramM = do
  CGState{..} <- get
  let prg = AbstractProgram
        { _absMemoryCounter   = _sMemoryCounter
        , _absRegisterCounter = _sRegisterCounter
        , _absInstructions    = _sInstructions
        }
      mapping = CByMapping
        { _producerMap  = _sProducerMap
        , _hptMapping   = AbstractMapping
            { _absRegisterMap     = _sRegisterMap
            , _absFunctionArgMap  = _sFunctionArgMap
            , _absTagMap          = _sTagMap
            }
        }
  pure (prg, mapping)

type Producer = IR.Int32

addProducer :: Name -> IR.Reg -> CG ()
addProducer v r = sProducerMap %= Map.insert r v

registerToProducer :: IR.Reg -> Producer
registerToProducer (IR.Reg r) = fromIntegral r

undefinedProducerName :: Name
undefinedProducerName = "#undefined"


codeGenNodeTypeCBy :: Tag -> Vector SimpleType -> CG IR.Reg
codeGenNodeTypeCBy tag ts = do
  irTag <- getTag tag
  r <- codeGenTaggedNodeType tag ts
  emit IR.Set {dstReg = r, constant = IR.CNodeItem irTag 0 undefinedProducer}
  pure r

codeGenVal :: Val -> CG IR.Reg
codeGenVal = \case
  ConstTagNode tag args -> do
    r <- newReg
    irTag <- getTag tag
    emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length args + 1)}
    emit IR.Set {dstReg = r, constant = IR.CNodeItem irTag 0 (registerToProducer r)}
    forM_ (zip [1..] args) $ \(idx, arg) -> do
      valReg <- getReg arg
      emit IR.Extend
        { srcReg      = valReg
        , dstSelector = IR.NodeItem irTag idx
        , dstReg      = r
        }
    pure r
  Unit -> do
    r <- newReg
    emit IR.Set {dstReg = r, constant = IR.CSimpleType (-1)}
    pure r
  Lit lit -> do
    r <- newReg
    emit IR.Set
      { dstReg    = r
      , constant  = IR.CSimpleType (litToSimpleType lit)
      }
    pure r
  Var name -> getReg name
  Undefined t -> codeGenType codeGenSimpleType (codeGenNodeSetWith codeGenNodeTypeCBy) t

typeTag :: Name -> Tag
typeTag n = Tag F n -- FIXME: this is a hack

projectType :: IR.Reg -> Ty -> CG [(Name, IR.Reg)]
projectType argReg = \case
  TySimple{}      -> pure []
  TyVar name      -> pure [(name, argReg)]
  TyCon name args -> do
    r <- newReg
    emit IR.Fetch {addressReg = argReg, dstReg = r}
    irTag <- getTag $ typeTag name
    fmap concat $ forM (zip [1..] args) $ \(idx, ty) -> do
      r1 <- newReg
      emit IR.Project {srcSelector = IR.NodeItem irTag idx, srcReg = r, dstReg = r1}
      projectType r1 ty

constructType :: [(Name, IR.Reg)] -> Ty -> CG IR.Reg
constructType argMap = \case
  TySimple simpleType -> do
    r <- newReg
    emit IR.Set {dstReg = r, constant = IR.CSimpleType (codegenSimpleType simpleType)}
    pure r
  TyVar name -> do
    r <- newReg
    mapM_ emit [IR.Move {srcReg = q, dstReg = r} | (n,q) <- argMap, n == name]
    pure r
  TyCon name args -> do
    -- construct type node
    valReg <- newReg
    irTag <- getTag $ typeTag name
    emit IR.Set {dstReg = valReg, constant = IR.CNodeType irTag (length args)}
    emit IR.Set {dstReg = valReg, constant = IR.CNodeItem irTag 0 undefinedProducer}
    -- fill type node componets
    forM_ (zip [1..] args) $ \(idx, ty) -> do
      q <- constructType argMap ty
      emit IR.Extend
        { srcReg      = q
        , dstSelector = IR.NodeItem irTag idx
        , dstReg      = valReg
        }
    -- store type node on abstract heap
    loc <- newMem
    r <- newReg
    emit IR.Store {srcReg = valReg, address = loc}
    emit IR.Set {dstReg = r, constant = IR.CHeapLocation loc}
    pure r

codeGenExternal :: External -> [Name] -> CG Result
codeGenExternal External{..} args = do
  valRegs <- mapM getReg args
  argMap <- concat <$> zipWithM projectType valRegs eArgsType
  R <$> constructType argMap eRetType

-- TODO: remove
codeGenProducer :: CG Result -> Name -> CG Result -> CG Result
codeGenProducer cgLeftExp prodName cgRightExp = do
  lResult <- cgLeftExp
  let R r = lResult
  addReg prodName r
  addProducer prodName r
  cgRightExp

producesNode :: Val -> Bool
producesNode ConstTagNode{} = True
producesNode (Undefined T_NodeSet{}) = True
producesNode _ = False

asPatternDataflow :: IR.Reg -> BPat -> CG ()
asPatternDataflow r asPat@(AsPat tag args _) = do
    irTag <- getTag tag
    bindInstructions <- forM (zip [1..] args) $ \(idx, arg) -> do
      argReg <- newReg
      addReg arg argReg
      pure [ IR.Project { srcReg      = r
                        , srcSelector = IR.NodeItem irTag idx
                        , dstReg      = argReg
                        }
           ]
    emit IR.If
      { condition     = IR.NodeTypeExists irTag
      , srcReg        = r
      , instructions  = concat bindInstructions
      }
asPatternDataflow _ pat = error $ "not @pattern: " ++ show (PP pat)

{- NOTE: para is needed to specify the order of evalution of the lhs and rhs on binds.
   paraM would execute both lhs and rhs before running the action that actually adds
   the variables to the scope (addReg).
-}
codeGen :: Exp -> (AbstractProgram, CByMapping)
codeGen e = flip evalState emptyCGState $ para folder e >> mkCByProgramM where
  folder :: ExpF (Exp, CG Result) -> CG Result
  folder = \case
    ProgramF exts defs -> do
      mapM_ addExternal exts
      mapM_ snd defs
      pure Z

    DefF name args (_,bodyRes) -> do
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      zipWithM_ addReg args funArgRegs
      bodyRes >>= \case
        Z   -> emit IR.Set {dstReg = funResultReg, constant = IR.CSimpleType unitType}
        R r -> emit IR.Move {srcReg = r, dstReg = funResultReg}
      pure Z

    -- NOTE: variable patterns
    EBindF (lhs, cgLhs) (VarPat var) (_, cgRhs) -> do
      lhsRes <- cgLhs
      case lhsRes of
        Z -> do
          r <- newReg
          addReg var r
        R r -> do
          case lhs of
            SReturn val | producesNode val -> do
              addReg var r
              addProducer var r
            _ -> addReg var r
      cgRhs

    -- NOTE: @patterns
    EBindF (lhs, cgLhs) asPat@(AsPat tag args var) (_, cgRhs) -> do
      lhsRes <- cgLhs
      case lhsRes of
        Z -> error $ "pattern mismatch at CreatedBy bind codegen, expected Unit got " ++ show (PP $ ConstTagNode tag args)
        R r -> do
          case lhs of
            SReturn val | producesNode val -> do
              addReg var r
              addProducer var r
              asPatternDataflow r asPat
            _ -> do
              addReg var r
              asPatternDataflow r asPat
      cgRhs

    ECaseF scrut alts_ -> do
      scrutReg      <- getReg scrut
      caseResultReg <- newReg
      altResults    <- sequence . fmap snd $ alts_

      forM_ altResults $ \(A cpat altNameReg altM) -> do
        let codeGenAlt bindM = codeGenBlock_ $ do
              bindM
              altM >>= \case
                Z -> emit IR.Set {dstReg = caseResultReg, constant = IR.CSimpleType unitType}   -- pure ()
                R altResultReg -> emit IR.Move {srcReg = altResultReg, dstReg = caseResultReg}

        case cpat of
          NodePat tag vars -> do
            irTag <- getTag tag
            altInstructions <- codeGenAlt $ do
              -- restrict scrutinee to alternative's domain
              altScrutReg <- newReg
              addReg scrut altScrutReg
              {- NOTE: We just create a new empty register, and associate it with the scrutinee in this alternative.
                Then we annotate the register with restricted properties of the scrutinee.
              -}
              emit IR.Project
                { srcSelector = IR.ConditionAsSelector $ IR.NodeTypeExists irTag
                , srcReg = scrutReg
                , dstReg = altScrutReg
                }
              emit IR.Move
                { srcReg = altScrutReg
                , dstReg = altNameReg
                }

              -- bind pattern variables
              forM_ (zip [1..] vars) $ \(idx, var) -> do
                argReg <- newReg
                addReg var argReg
                emit IR.Project
                  { srcSelector = IR.NodeItem irTag idx
                  , srcReg      = scrutReg
                  , dstReg      = argReg
                  }
            emit IR.If
              { condition    = IR.NodeTypeExists irTag
              , srcReg       = scrutReg
              , instructions = altInstructions
              }

          LitPat lit -> do
            altInstructions <- codeGenAlt $ do
              -- restrict scrutinee to alternative's domain
              altScrutReg <- newReg
              addReg scrut altScrutReg
              emit IR.Project
                { srcSelector = IR.ConditionAsSelector $ IR.SimpleTypeExists (litToSimpleType lit)
                , srcReg = scrutReg
                , dstReg = altScrutReg
                }
              emit IR.Move
                { srcReg = altScrutReg
                , dstReg = altNameReg
                }
            emit IR.If
              { condition    = IR.SimpleTypeExists (litToSimpleType lit)
              , srcReg       = scrutReg
              , instructions = altInstructions
              }

          DefaultPat -> do
            tags <- Set.fromList <$> sequence [getTag tag | A (NodePat tag _) _ _ <- altResults]
            altInstructions <- codeGenAlt $ do
              -- restrict scrutinee to alternative's domain
              altScrutReg <- newReg
              addReg scrut altScrutReg
              emit IR.Project
                { srcSelector = IR.ConditionAsSelector $ IR.AnyNotIn tags
                , srcReg      = scrutReg
                , dstReg      = altScrutReg
                }
              emit IR.Move
                { srcReg = altScrutReg
                , dstReg = altNameReg
                }
            emit IR.If
              { condition    = IR.AnyNotIn tags
              , srcReg       = scrutReg
              , instructions = altInstructions
              }

      -- restore scrutinee register mapping
      addReg scrut scrutReg

      pure $ R caseResultReg

    {- NOTE: The alternatives are already evaluated,
       we only have return them.
    -}
    AltF cpat n (_, cgAlt) -> do
      altNameReg <- newReg
      addReg n altNameReg
      pure $ A cpat altNameReg cgAlt

    SAppF name args -> getExternal name >>= \case
      Just ext  -> do
        res <- codeGenExternal ext args
        let R r = res
        -- HINT: workaround
        -----------
        -- copy args to definition's variables ; read function result register
        (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
        argRegs <- mapM getReg args
        zipWithM_ (\src dst -> emit IR.Move {srcReg = src, dstReg = dst}) argRegs funArgRegs
        -- old prim codegen
        let External{..} = ext
            isTySimple TySimple{} = True
            isTySimple _ = False
        emit IR.Move {srcReg = r, dstReg = funResultReg}
        when (isTySimple eRetType && all isTySimple eArgsType) $ do
          zipWithM_ (\argReg (TySimple argTy) -> emit IR.Set {dstReg = argReg, constant = IR.CSimpleType (codegenSimpleType argTy)}) funArgRegs eArgsType

        pure res

        -----------

      Nothing   -> do
        -- copy args to definition's variables ; read function result register
        (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
        argRegs <- mapM getReg args
        zipWithM_ (\src dst -> emit IR.Move {srcReg = src, dstReg = dst}) argRegs funArgRegs
        pure $ R funResultReg


    SReturnF val -> R <$> codeGenVal val

    SStoreF var -> do
      loc <- newMem
      r <- newReg
      varReg <- getReg var
      emit IR.Store {srcReg = varReg, address = loc}
      emit IR.Set {dstReg = r, constant = IR.CHeapLocation loc}
      pure $ R r

    SFetchF ptr -> do
      ptrReg <- getReg ptr
      r <- newReg
      emit IR.Fetch {addressReg = ptrReg, dstReg = r}
      pure $ R r

    SUpdateF ptr var -> do
      ptrReg <- getReg ptr
      varReg <- getReg var
      emit IR.Update {srcReg = varReg, addressReg = ptrReg}
      pure Z

    SBlockF (_, cgBlock) -> cgBlock
