{-# LANGUAGE LambdaCase, TupleSections #-}
module AbstractInterpretation.CreatedBy
  ( codeGen ) where

import Control.Monad.Trans.Except
import Control.Monad.State

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Functor.Foldable as Foldable

import Grin.Grin
import AbstractInterpretation.CodeGen
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (Instruction(..), HPTProgram(..), emptyHPTProgram)
import AbstractInterpretation.HeapPointsTo (unitType, litToSimpleType, codeGenPrimOp)


-- type CreatedByProdMap = Map.Map Name Name
-- type CreatedByProgram = (CreatedByProdMap, HPTProgram)

addProducer :: Name -> CG ()
addProducer x = pure ()

registerToProducer :: IR.Reg -> IR.Producer
registerToProducer (IR.Reg r) = fromIntegral r

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
        emit IR.Extend {srcReg = valReg, dstSelector = IR.NodeItem irTag idx, dstReg = r}
      Lit lit -> emit IR.Set {dstReg = r, constant = IR.CNodeItem irTag idx (litToSimpleType lit)}
      _ -> throwE $ "illegal node item value " ++ show val
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
  val -> throwE $ "unsupported value " ++ show val

codeGen :: Exp -> Either String HPTProgram
codeGen = (\(a,s) -> s<$a) . flip runState IR.emptyHPTProgram . runExceptT . para folder where
  folder :: ExpF (Exp, CG Result) -> CG Result
  folder = \case
    ProgramF defs -> (sequence_ . fmap snd $ defs) >> pure Z

    DefF name args (_,body) -> do
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      zipWithM_ addReg args funArgRegs
      body >>= \case
        Z   -> emit IR.Set {dstReg = funResultReg, constant = IR.CSimpleType unitType}
        R r -> emit IR.Move {srcReg = r, dstReg = funResultReg}
      pure Z

    EBindF (SReturn ConstTagNode{},leftExp) (Var v) (_,rightExp) -> do
      R r <- leftExp
      addReg v r
      addProducer v
      rightExp
    EBindF (_,leftExp) lpat (_,rightExp) -> do
      leftExp >>= \case
        Z -> case lpat of
          Unit -> pure ()
          Var name -> do
            r <- newReg
            emit IR.Set {dstReg = r, constant = IR.CSimpleType unitType}
            addReg name r
          _ -> throwE $ "pattern mismatch at CreatedBy bind codegen, expected Unit got " ++ show lpat
        R r -> case lpat of -- QUESTION: should the evaluation continue if the pattern does not match yet?
          Unit  -> pure () -- TODO: is this ok? or error?
          -- NOTE: I think this is okay. Could be optimised though (since we already know the result)?
          Lit{} -> pure () -- TODO: is this ok? or error?
          Var name -> addReg name r
          ConstTagNode tag args -> do
            irTag <- getTag tag
            bindInstructions <- forM (zip [0..] args) $ \(idx, arg) -> case arg of
              Var name -> do
                argReg <- newReg
                addReg name argReg
                pure [IR.Project {srcSelector = IR.NodeItem irTag idx, srcReg = r, dstReg = argReg}]
              Lit {} -> pure []
              _ -> throwE $ "illegal node pattern component " ++ show arg
            -- QUESTION: In HPTProgram the instructions are in reverse order, here they are in regular order, isn't this inconsistent?
            emit IR.If
              { condition     = IR.NodeTypeExists irTag
              , srcReg        = r
              , instructions  = concat bindInstructions
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
      {-
        TODO:
          - create scope monadic combinator to handle scopes
          - set scrutinee value to the case alternative pattern value in the alternative scope
      -}
      alts <- sequence . fmap snd $ alts_

      forM_ alts $ \(A cpat altM) -> do
        let codeGenAlt bindM = codeGenBlock $ do
              bindM
              altM >>= \case
                Z -> emit IR.Set {dstReg = caseResultReg, constant = IR.CSimpleType unitType}   -- pure ()
                R altResultReg -> emit IR.Move {srcReg = altResultReg, dstReg = caseResultReg}

        case cpat of
          NodePat tag vars -> do
            irTag <- getTag tag
            altInstructions <- codeGenAlt $ do
              -- restrict scrutinee to alternative's domain
              flip (maybe (pure ())) scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                -- NOTE: We just create a new empty register, and associate it with the scrutinee in this alternative. Then we annotate the register with restricted properties of the scrutinee.
                emit IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.NodeTypeExists irTag
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }

              -- bind pattern variables
              forM_ (zip [0..] vars) $ \(idx, name) -> do
                  argReg <- newReg
                  addReg name argReg
                  emit IR.Project {srcSelector = IR.NodeItem irTag idx, srcReg = valReg, dstReg = argReg}
            emit IR.If {condition = IR.NodeTypeExists irTag, srcReg = valReg, instructions = altInstructions}

          -- QUESTION: should we store simple types to have more information?
          LitPat lit -> do
            altInstructions <- codeGenAlt $
              -- restrict scrutinee to alternative's domain
              flip (maybe (pure ())) scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                emit IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.SimpleTypeExists (litToSimpleType lit)
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }
            -- QUESTION: Redundant IF. Just for consistency?
            emit IR.If {condition = IR.SimpleTypeExists (litToSimpleType lit), srcReg = valReg, instructions = altInstructions}

          DefaultPat -> do
            tags <- Set.fromList <$> sequence [getTag tag | A (NodePat tag _) _ <- alts]
            altInstructions <- codeGenAlt $
              -- restrict scrutinee to alternative's domain
              flip (maybe (pure ())) scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                emit IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.NotIn tags
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }
            -- QUESTION: Redundant IF. Just for consistency?
            emit IR.If {condition = IR.NotIn tags, srcReg = valReg, instructions = altInstructions}

          _ -> throwE $ "HPT does not support the following case pattern: " ++ show cpat

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
      Just {} -> throwE "CBy codegen does not support indexed fetch"
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
