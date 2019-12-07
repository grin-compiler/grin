{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, TemplateHaskell, OverloadedStrings #-}
module AbstractInterpretation.HeapPointsTo.CodeGen where

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
import Grin.TypeEnv
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (Instruction(..), AbstractProgram(..), AbstractMapping(..))
import AbstractInterpretation.HeapPointsTo.CodeGenBase

codeGen :: Program -> (AbstractProgram, HPTMapping)
codeGen prg@(Program exts defs) = evalState (codeGenM prg >> mkAbstractProgramM) emptyCGState
codeGen _ = error "Program expected"

mkAbstractProgramM :: CG (AbstractProgram, HPTMapping)
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

unitType :: IR.SimpleType
unitType = codegenSimpleType T_Unit
{-
type ValueMapping = (Map Typeable Int, Int)
type MappingM = State ValueMapping

class (Typeable a, Enum a, Bounded a) => IntValue a where
  toInt   :: Monad m => a -> m Int
  fromInt :: Monad m => Int -> m a
-}
codegenSimpleType :: SimpleType -> IR.SimpleType
codegenSimpleType = \case
  T_Unit    -> -1
  T_Int64   -> -2
  T_Word64  -> -3
  T_Float   -> -4
  T_Bool    -> -5
  T_String  -> -6
  T_Char    -> -7

litToSimpleType :: Lit -> IR.SimpleType
litToSimpleType = codegenSimpleType . typeOfLitST

codeGenNodeTypeHPT :: Tag -> Vector SimpleType -> CG IR.Reg
codeGenNodeTypeHPT tag ts = do
  let ts' = Vec.toList ts
  r <- newReg
  irTag <- getTag tag
  argRegs <- mapM codeGenSimpleType ts'
  emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length argRegs)}
  forM_ (zip [0..] argRegs) $ \(idx, argReg) ->
    emit IR.Extend {srcReg = argReg, dstSelector = IR.NodeItem irTag idx, dstReg = r}
  pure r

codeGenVal :: Val -> CG IR.Reg
codeGenVal = \case
  ConstTagNode tag vals -> do
    r <- newReg
    irTag <- getTag tag
    emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length vals)}
    forM_ (zip [0..] vals) $ \(idx, val) -> case val of
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
  Undefined t -> codeGenType codeGenSimpleType (codeGenNodeSetWith codeGenNodeTypeHPT) t
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
    fmap concat $ forM (zip [0..] args) $ \(idx, ty) -> do
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
    -- fill type node componets
    forM_ (zip [0..] args) $ \(idx, ty) -> do
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
  r <- constructType argMap eRetType
  pure $ R r

codeGenM :: Exp -> CG Result
codeGenM = cata folder where
  folder :: ExpF (CG Result) -> CG Result
  folder = \case
    ProgramF exts defs -> mapM_ addExternal exts >> sequence_ defs >> pure Z

    DefF name args body -> do
      instructions <- state $ \s@CGState{..} -> (_sInstructions, s {_sInstructions = []})
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      zipWithM_ addReg args funArgRegs
      body >>= \case
        Z   -> emit IR.Set {dstReg = funResultReg, constant = IR.CSimpleType unitType}
        R r -> emit IR.Move {srcReg = r, dstReg = funResultReg}
      -- QUESTION: why do we reverse?
      -- A: because the list is built in reversed order (cons) and it is more efficient to reverse only once
      modify' $ \s@CGState{..} -> s {_sInstructions = reverse _sInstructions ++ instructions}
      pure Z

    EBindF leftExp lpat rightExp -> do
      leftExp >>= \case
        Z -> case lpat of
          Unit -> pure ()
          Var name -> do
            r <- newReg
            emit IR.Set {dstReg = r, constant = IR.CSimpleType unitType}
            addReg name r
          _ -> error $ "pattern mismatch at HPT bind codegen, expected Unit got " ++ show lpat
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
              _ -> error $ "illegal node pattern component " ++ show arg
            -- QUESTION: In HPTProgram the instructions are in reverse order, here they are in regular order, isn't this inconsistent?
            -- A: each cpat argument has zero or one instruction
            --    the order of cpat binding evaluation does not matter because they does not depend on each other
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
      alts <- sequence alts_

      forM_ alts $ \(A cpat altM) -> do
        let codeGenAlt bindM = codeGenBlock_ $ do
              bindM
              altM >>= \case
                Z -> emit IR.Set {dstReg = caseResultReg, constant = IR.CSimpleType unitType}
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
              forM_ (zip [0..] vars) $ \(idx, name) -> do
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
            -- QUESTION: Redundant IF. Just for consistency?
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
            -- QUESTION: Redundant IF. Just for consistency?
            emit IR.If {condition = IR.AnyNotIn tags, srcReg = valReg, instructions = altInstructions}

          _ -> error $ "HPT does not support the following case pattern: " ++ show cpat

      -- restore scrutinee register mapping
      maybe (pure ()) (uncurry addReg) scrutRegMapping

      pure $ R caseResultReg

    AltF cpat exp -> pure $ A cpat exp

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
      Just {} -> error "HPT codegen does not support indexed fetch"
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

    SBlockF exp -> exp

{-
  Unit      -1
  Int       -2
  Word      -3
  Float     -4
  Bool      -5
  Undefined -9999
-}

{-
 CONSTANT value building operations (from compile time constants)
  add simple type
  add heap location
  add node type (tag + arity)
  add node item (index + location or simple type)
-}

{-
  >>= LPAT
    (Tag b c d)               - node only check, arity check, tag check, copy node items to registers for a specific tag
    Unit                      - specific simple type only check
    Literal with simple type  - specific simple type only check
    variable                  - copy to reg

  case >>= LPAT ()
  return >>= LPAT ()
  store >>= LPAT
  fetch >>= LPAT
  update >>= LPAT

  LPAT / VAL
  T a - select items of specific tag
  Unit- NOP ; optional check
  Lit - NOP ; optional check
  a   - copy all


 bind COMPILE TIME CHECK:
  VAL / LPAT
  T a - T a   OK      (node)
      - Unit  FAIL
      - Lit   FAIL
      - a     OK

  Unit- T a   FAIL    (simple type)
      - Unit  OK
      - Lit   FAIL
      - a     OK

  Lit - T a   FAIL    (simple type)
      - Unit  FAIL
      - Lit   OK ; if matches
      - a     OK

  a   - T a   OK      (any)
      - Unit  OK
      - Lit   OK
      - a     OK

  LOC - T a   FAIL    (heap location)
      - Unit  FAIL
      - Lit   FAIL
      - a     OK

  compilation:
    store (the only valid expression): store VAL >>= var
      emits:
        one time constant register setup for var
        memory copy or one time setup
        VAL validation
    update DST VAL >>= (var | Unit)
      VAL validation
      DST validaton
      var: emits one time setup for var ; fill with Unit type
      Unit: nothing to do
    fetch SRC >>= (var | (T a) | (t a))
      SRC validation
      var: CopyMemReg
      (T a):

-}

{-
 case COMPILE TIME CHECK:
  VAL / CPAT
  T a - T a   OK      (node)
      - Lit   FAIL

  Unit- T a   FAIL    (simple type)
      - Lit   FAIL

  Lit - T a   FAIL    (simple type)
      - Lit   OK ; if matches

  a   - T a   OK      (any)
      - Lit   OK

  LOC - T a   FAIL    (heap location)
      - Lit   FAIL
-}
