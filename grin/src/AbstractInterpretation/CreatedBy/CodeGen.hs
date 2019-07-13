{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module AbstractInterpretation.CreatedBy.CodeGen where

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

import Grin.Grin
import Grin.TypeEnvDefs
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (Instruction(..), AbstractProgram(..), emptyAbstractProgram, AbstractMapping(..))
import AbstractInterpretation.CreatedBy.CodeGenBase
import AbstractInterpretation.HeapPointsTo.CodeGen (litToSimpleType, unitType, codegenSimpleType) -- FIXME: why? remove, refactor
import AbstractInterpretation.HeapPointsTo.Result (undefinedProducer) -- FIXME: why? remove, refactor

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

addProducer :: IR.Reg -> Name -> CG ()
addProducer r v = sProducerMap %= Map.insert r v

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
  ConstTagNode tag vals -> do
    r <- newReg
    irTag <- getTag tag
    emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length vals + 1)}
    emit IR.Set {dstReg = r, constant = IR.CNodeItem irTag 0 (registerToProducer r)}
    forM_ (zip [1..] vals) $ \(idx, val) -> case val of
      Var name -> do
        valReg <- getReg name
        emit IR.Extend
          { srcReg      = valReg
          , dstSelector = IR.NodeItem irTag idx
          , dstReg      = r
          }
      Lit lit -> emit IR.Set {dstReg = r, constant = IR.CNodeItem irTag idx (litToSimpleType lit)}
      Undefined (T_SimpleType t) -> do
        tmp <- codeGenSimpleType t
        emit IR.Extend
          { srcReg      = tmp
          , dstSelector = IR.NodeItem irTag idx
          , dstReg      = r
          }
      _ -> error $ "illegal node item value " ++ show val
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
  val -> error $ "unsupported value " ++ show val

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

codeGenExternal :: External -> [Val] -> CG Result
codeGenExternal External{..} args = do
  valRegs <- mapM codeGenVal args
  argMap <- concat <$> zipWithM projectType valRegs eArgsType
  R <$> constructType argMap eRetType

codeGen :: Exp -> (AbstractProgram, CByMapping)
codeGen e = flip evalState emptyCGState $ para folder e >> mkCByProgramM where
  folder :: ExpF (Exp, CG Result) -> CG Result
  folder = \case
    ProgramF exts defs -> do
      mapM_ addExternal exts
      mapM_ snd defs
      pure Z

    DefF name args (_,body) -> do
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      zipWithM_ addReg args funArgRegs
      body >>= \case
        Z   -> emit IR.Set {dstReg = funResultReg, constant = IR.CSimpleType unitType}
        R r -> emit IR.Move {srcReg = r, dstReg = funResultReg}
      pure Z

    EBindF (SReturn lhs,leftExp) (Var v) (_,rightExp)
      | ConstTagNode{}        <- lhs -> cgProducer leftExp v rightExp
      | Undefined T_NodeSet{} <- lhs -> cgProducer leftExp v rightExp
      where
        cgProducer lExp p rExp = do
          reg <- lExp
          let R r = reg
          addReg v r
          addProducer r v
          rExp
    EBindF (_,leftExp) lpat (_,rightExp) -> do
      leftExp >>= \case
        Z -> case lpat of
          Unit -> pure ()
          Var name -> do
            r <- newReg
            emit IR.Set {dstReg = r, constant = IR.CSimpleType unitType}
            addReg name r
          _ -> error $ "pattern mismatch at CreatedBy bind codegen, expected Unit got " ++ show lpat
        R r -> case lpat of -- QUESTION: should the evaluation continue if the pattern does not match yet?
          Unit  -> pure () -- TODO: is this ok? or error?
          Lit{} -> pure () -- TODO: is this ok? or error?
          Var name -> addReg name r
          ConstTagNode tag args -> do
            irTag <- getTag tag
            bindInstructions <- forM (zip [1..] args) $ \(idx, arg) -> case arg of
              Var name -> do
                argReg <- newReg
                addReg name argReg
                pure [ IR.Project { srcReg      = r
                                  , srcSelector = IR.NodeItem irTag idx
                                  , dstReg      = argReg
                                  }
                     ]
              Lit {} -> pure []
              _ -> error $ "illegal node pattern component " ++ show arg
            emit IR.If
              { condition     = IR.NodeTypeExists irTag
              , srcReg        = r
              , instructions  = concat bindInstructions
              }
          _ -> error $ "unsupported lpat " ++ show lpat
      rightExp

    ECaseF val alts_ -> do
      valReg <- codeGenVal val
      caseResultReg <- newReg

      -- save scrutinee register mapping
      scrutRegMapping <- case val of
        Var name -> Just . (name,) <$> getReg name
        _ -> pure Nothing
      {-
        TODO:
          - create scope monadic combinator to handle scopes
          - set scrutinee value to the case alternative pattern value in the alternative scope
      -}
      alts <- sequence . fmap snd $ alts_

      forM_ alts $ \(A cpat altM) -> do
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
              forM_ scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                -- NOTE: We just create a new empty register, and associate it with the scrutinee in this alternative. Then we annotate the register with restricted properties of the scrutinee.
                emit IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.NodeTypeExists irTag
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }

              -- bind pattern variables
              forM_ (zip [1..] vars) $ \(idx, name) -> do
                  argReg <- newReg
                  addReg name argReg
                  emit IR.Project {srcSelector = IR.NodeItem irTag idx, srcReg = valReg, dstReg = argReg}
            emit IR.If {condition = IR.NodeTypeExists irTag, srcReg = valReg, instructions = altInstructions}

          LitPat lit -> do
            altInstructions <- codeGenAlt $
              -- restrict scrutinee to alternative's domain
              forM_ scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                emit IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.SimpleTypeExists (litToSimpleType lit)
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }
            emit IR.If {condition = IR.SimpleTypeExists (litToSimpleType lit), srcReg = valReg, instructions = altInstructions}

          DefaultPat -> do
            tags <- Set.fromList <$> sequence [getTag tag | A (NodePat tag _) _ <- alts]
            altInstructions <- codeGenAlt $
              -- restrict scrutinee to alternative's domain
              forM_ scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                emit IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.AnyNotIn tags
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }
            emit IR.If {condition = IR.AnyNotIn tags, srcReg = valReg, instructions = altInstructions}

          _ -> error $ "CBy does not support the following case pattern: " ++ show cpat

      -- restore scrutinee register mapping
      maybe (pure ()) (uncurry addReg) scrutRegMapping

      pure $ R caseResultReg

    AltF cpat (_,exp) -> pure $ A cpat exp

    SAppF name args -> getExternal name >>= \case
      Just ext  -> do
        res <- codeGenExternal ext args
        let R r = res
        -- HINT: workaround
        -----------
        -- copy args to definition's variables ; read function result register
        (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
        valRegs <- mapM codeGenVal args
        zipWithM_ (\src dst -> emit IR.Move {srcReg = src, dstReg = dst}) valRegs funArgRegs
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
        valRegs <- mapM codeGenVal args
        zipWithM_ (\src dst -> emit IR.Move {srcReg = src, dstReg = dst}) valRegs funArgRegs
        pure $ R funResultReg


    SReturnF val -> R <$> codeGenVal val

    SStoreF val -> do
      loc <- newMem
      r <- newReg
      valReg <- codeGenVal val
      emit IR.Store {srcReg = valReg, address = loc}
      emit IR.Set {dstReg = r, constant = IR.CHeapLocation loc}
      pure $ R r

    SFetchIF name maybeIndex -> case maybeIndex of
      Just {} -> error "CBy codegen does not support indexed fetch"
      Nothing -> do
        addressReg <- getReg name
        r <- newReg
        emit IR.Fetch {addressReg = addressReg, dstReg = r}
        pure $ R r

    SUpdateF name val -> do
      addressReg <- getReg name
      valReg <- codeGenVal val
      emit IR.Update {srcReg = valReg, addressReg = addressReg}
      pure Z

    SBlockF (_,exp) -> exp
