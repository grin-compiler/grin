{-# LANGUAGE LambdaCase, TupleSections, TemplateHaskell, OverloadedStrings #-}
module AbstractInterpretation.CreatedBy.CodeGen where

import Control.Monad.Trans.Except
import Control.Monad.State

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Data.Functor.Foldable as Foldable

import Lens.Micro.Platform
import Lens.Micro.Internal

import Grin.Grin
import Grin.TypeEnvDefs
import AbstractInterpretation.CodeGen
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (Instruction(..), AbstractProgram(..), HasDataFlowInfo(..))
import AbstractInterpretation.HeapPointsTo.CodeGen (HPTProgram, emptyHPTProgram, litToSimpleType, unitType, codeGenPrimOp) -- FIXME: why? remove, refactor


-- HPT program with producer information about nodes
-- for each node, it contains the node's possible producers in the first field
newtype HPTWProducerInfo = HPTProducerInfo { _hptProg :: HPTProgram } deriving (Show)

data CByProgram =
  CByProgram
  { _producerMap  :: Map.Map IR.Reg Name
  , _hptProgWProd :: HPTWProducerInfo
  } deriving (Show)
concat <$> mapM makeLenses [''CByProgram, ''HPTWProducerInfo]

instance HasDataFlowInfo CByProgram where
  dataFlowInfo = hptProgWProd.hptProg.dataFlowInfo

emptyCByProgram :: CByProgram
emptyCByProgram = CByProgram Map.empty (HPTProducerInfo emptyHPTProgram)

type ResultCBy = Result CByProgram
type Producer = IR.Int32

throwCBy :: (Monad m) => String -> ExceptT String m a
throwCBy s = throwE $ "CBy: " ++ s

addProducer :: IR.Reg -> Name -> CG CByProgram ()
addProducer r v = producerMap %= Map.insert r v

registerToProducer :: IR.Reg -> Producer
registerToProducer (IR.Reg r) = fromIntegral r

undefinedProducer :: Producer
undefinedProducer = -1723

undefinedProducerName :: Name
undefinedProducerName = "#undefined"


codeGenNodeTypeCBy :: HasDataFlowInfo s =>
                      Tag -> Vector SimpleType -> CG s IR.Reg
codeGenNodeTypeCBy tag ts = do
  irTag <- getTag tag
  r <- codeGenTaggedNodeType tag ts
  emit IR.Set {dstReg = r, constant = IR.CNodeItem irTag 0 undefinedProducer}
  pure r

codeGenVal :: Val -> CG CByProgram IR.Reg
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
      _ -> throwCBy $ "illegal node item value " ++ show val
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
  val -> throwCBy $ "unsupported value " ++ show val

codeGen :: Exp -> Either String CByProgram
codeGen = (\(a,s) -> s<$a) . flip runState emptyCByProgram . runExceptT . para folder where
  folder :: ExpF (Exp, CG CByProgram ResultCBy) -> CG CByProgram ResultCBy
  folder = \case
    ProgramF exts defs -> (sequence_ . fmap snd $ defs) >> pure Z

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
          _ -> throwCBy $ "pattern mismatch at CreatedBy bind codegen, expected Unit got " ++ show lpat
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
              _ -> throwCBy $ "illegal node pattern component " ++ show arg
            emit IR.If
              { condition     = IR.NodeTypeExists irTag
              , srcReg        = r
              , instructions  = concat bindInstructions
              }
          _ -> throwCBy $ "unsupported lpat " ++ show lpat
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
                  { srcSelector = IR.ConditionAsSelector $ IR.NotIn tags
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }
            emit IR.If {condition = IR.NotIn tags, srcReg = valReg, instructions = altInstructions}

          _ -> throwCBy $ "CBy does not support the following case pattern: " ++ show cpat

      -- restore scrutinee register mapping
      maybe (pure ()) (uncurry addReg) scrutRegMapping

      pure $ R caseResultReg

    AltF cpat (_,exp) -> pure $ A cpat exp

    SAppF name args -> do -- copy args to definition's variables ; read function result register
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      valRegs <- mapM codeGenVal args
      zipWithM_ (\src dst -> emit IR.Move {srcReg = src, dstReg = dst}) valRegs funArgRegs
      -- HINT: handle primop here because it does not have definition
      when (isPrimName name) $ codeGenPrimOp name funResultReg funArgRegs
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
      Just {} -> throwCBy "CBy codegen does not support indexed fetch"
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
