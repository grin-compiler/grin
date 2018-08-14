{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections #-}
module AbstractInterpretation.CodeGen
  ( codeGen
  ) where

import Control.Monad.Trans.Except
import Data.Word
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Functor.Foldable as Foldable

import Grin.Grin
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (HPTProgram(..), emptyHPTProgram)

type CG = ExceptT String (State HPTProgram)

data Result
  = R IR.Reg
  | Z
  | A CPat (CG Result)


emit :: IR.Instruction -> CG ()
emit inst = modify' $ \s@HPTProgram{..} -> s {hptInstructions = inst : hptInstructions}

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

-- creates regsiters for function arguments and result
getOrAddFunRegs :: Name -> Int -> CG (IR.Reg, [IR.Reg])
getOrAddFunRegs name arity = do
  funMap <- gets hptFunctionArgMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      resReg <- newReg
      argRegs <- replicateM arity newReg
      let funRegs = (resReg, argRegs)
      modify' $ \s@HPTProgram{..} -> s {hptFunctionArgMap = Map.insert name funRegs hptFunctionArgMap}
      pure funRegs

newReg :: CG IR.Reg
newReg = state $ \s@HPTProgram{..} -> (IR.Reg hptRegisterCounter, s {hptRegisterCounter = succ hptRegisterCounter})

newMem :: CG IR.Mem
newMem = state $ \s@HPTProgram{..} -> (IR.Mem hptMemoryCounter, s {hptMemoryCounter = succ hptMemoryCounter})

addReg :: Name -> IR.Reg -> CG ()
addReg name reg = modify' $ \s@HPTProgram{..} -> s {hptRegisterMap = Map.insert name reg hptRegisterMap}

getReg :: Name -> CG IR.Reg
getReg name = do
  regMap <- gets hptRegisterMap
  case Map.lookup name regMap of
    Nothing   -> throwE $ "unknown variable " ++ name
    Just reg  -> pure reg

getTag :: Tag -> CG IR.Tag
getTag tag = do
  tagMap <- gets hptTagMap
  case Bimap.lookup tag tagMap of
    Just t  -> pure t
    Nothing -> do
      let t = IR.Tag . fromIntegral $ Bimap.size tagMap
      modify' $ \s -> s {hptTagMap = Bimap.insert tag t tagMap}
      pure t

unitType :: IR.SimpleType
unitType = -1

litToSimpleType :: Lit -> IR.SimpleType
litToSimpleType = \case
  LInt64  {}  -> -2
  LWord64 {}  -> -3
  LFloat  {}  -> -4
  LBool   {}  -> -5

codeGenBlock :: CG () -> CG [IR.Instruction]
codeGenBlock genM = do
  instructions <- state $ \s@HPTProgram{..} -> (hptInstructions, s {hptInstructions = []})
  genM
  blockInstructions <- state $ \s@HPTProgram{..} -> (reverse hptInstructions, s {hptInstructions = instructions})
  pure blockInstructions

codeGenVal :: Val -> CG IR.Reg
codeGenVal = \case
  ConstTagNode tag vals -> do
    r <- newReg
    irTag <- getTag tag
    emit $ IR.Set {dstReg = r, constant = IR.CNodeType irTag (length vals)}
    forM_ (zip [0..] vals) $ \(idx, val) -> case val of
      Var name -> do
        valReg <- getReg name
        emit $ IR.Extend {srcReg = valReg, dstSelector = IR.NodeItem irTag idx, dstReg = r}
      Lit lit -> emit $ IR.Set {dstReg = r, constant = IR.CNodeItem irTag idx (litToSimpleType lit)}
      _ -> throwE $ "illegal node item value " ++ show val
    pure r
  Unit -> do
    r <- newReg
    emit $ IR.Set {dstReg = r, constant = IR.CSimpleType (-1)}
    pure r
  Lit lit -> do
    r <- newReg
    emit $ IR.Set
      { dstReg    = r
      , constant  = IR.CSimpleType (litToSimpleType lit)
      }
    pure r
  Var name -> getReg name
  val -> throwE $ "unsupported value " ++ show val

codeGenPrimOp :: Name -> IR.Reg -> [IR.Reg] -> CG ()
codeGenPrimOp name funResultReg funArgRegs = do
  let op argTypes resultTy = do
        emit $ IR.Set {dstReg = funResultReg, constant = IR.CSimpleType resultTy}
        zipWithM_ (\argReg argTy -> emit $ IR.Set {dstReg = argReg, constant = IR.CSimpleType argTy}) funArgRegs argTypes

      unit  = -1
      int   = litToSimpleType $ LInt64 0
      word  = litToSimpleType $ LWord64 0
      float = litToSimpleType $ LFloat 0
      bool  = litToSimpleType $ LBool False

  case name of
    "_prim_int_print" -> op [int] unit
    -- Int
    "_prim_int_add"   -> op [int, int] int
    "_prim_int_sub"   -> op [int, int] int
    "_prim_int_mul"   -> op [int, int] int
    "_prim_int_div"   -> op [int, int] int
    "_prim_int_eq"    -> op [int, int] bool
    "_prim_int_ne"    -> op [int, int] bool
    "_prim_int_gt"    -> op [int, int] bool
    "_prim_int_ge"    -> op [int, int] bool
    "_prim_int_lt"    -> op [int, int] bool
    "_prim_int_le"    -> op [int, int] bool
    -- Word
    "_prim_word_add"  -> op [word, word] word
    "_prim_word_sub"  -> op [word, word] word
    "_prim_word_mul"  -> op [word, word] word
    "_prim_word_div"  -> op [word, word] word
    "_prim_word_eq"   -> op [word, word] bool
    "_prim_word_ne"   -> op [word, word] bool
    "_prim_word_gt"   -> op [word, word] bool
    "_prim_word_ge"   -> op [word, word] bool
    "_prim_word_lt"   -> op [word, word] bool
    "_prim_word_le"   -> op [word, word] bool
    -- Float
    "_prim_float_add" -> op [float, float] float
    "_prim_float_sub" -> op [float, float] float
    "_prim_float_mul" -> op [float, float] float
    "_prim_float_div" -> op [float, float] float
    "_prim_float_eq"  -> op [float, float] bool
    "_prim_float_ne"  -> op [float, float] bool
    "_prim_float_gt"  -> op [float, float] bool
    "_prim_float_ge"  -> op [float, float] bool
    "_prim_float_lt"  -> op [float, float] bool
    "_prim_float_le"  -> op [float, float] bool
    -- Bool
    "_prim_bool_eq"   -> op [bool, bool] bool
    "_prim_bool_ne"   -> op [bool, bool] bool


codeGen :: Exp -> Either String HPTProgram
codeGen = (\(a,s) -> s<$a) . flip runState IR.emptyHPTProgram . runExceptT . cata folder where
  folder :: ExpF (CG Result) -> CG Result
  folder = \case
    ProgramF defs -> sequence_ defs >> pure Z

    DefF name args body -> do
      instructions <- state $ \s@HPTProgram{..} -> (hptInstructions, s {hptInstructions = []})
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      zipWithM addReg args funArgRegs
      body >>= \case
        Z   -> emit $ IR.Set {dstReg = funResultReg, constant = IR.CSimpleType unitType}
        R r -> emit $ IR.Move {srcReg = r, dstReg = funResultReg}
      -- QUESTION: why do we reverse?
      modify' $ \s@HPTProgram{..} -> s {hptInstructions = reverse hptInstructions ++ instructions}
      pure Z

    EBindF leftExp lpat rightExp -> do
      leftExp >>= \case
        Z -> case lpat of
          Unit -> pure ()
          Var name -> do
            r <- newReg
            emit $ IR.Set {dstReg = r, constant = IR.CSimpleType unitType}
            addReg name r
          _ -> throwE $ "pattern mismatch at HPT bind codegen, expected Unit got " ++ show lpat
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
            emit $ IR.If
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
      alts <- sequence alts_

      forM_ alts $ \(A cpat altM) -> do
        let codeGenAlt bindM = codeGenBlock $ do
              bindM
              altM >>= \case
                Z -> emit $ IR.Set {dstReg = caseResultReg, constant = IR.CSimpleType unitType}
                R altResultReg -> emit $ IR.Move {srcReg = altResultReg, dstReg = caseResultReg}

        case cpat of
          NodePat tag vars -> do
            irTag <- getTag tag
            altInstructions <- codeGenAlt $ do
              -- restrict scrutinee to alternative's domain
              flip (maybe (pure ())) scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                -- NOTE: We just create a new empty register, and associate it with the scrutinee in this alternative. Then we annotate the register with restricted properties of the scrutinee.
                emit $ IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.NodeTypeExists irTag
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }

              -- bind pattern variables
              forM_ (zip [0..] vars) $ \(idx, name) -> do
                  argReg <- newReg
                  addReg name argReg
                  emit $ IR.Project {srcSelector = IR.NodeItem irTag idx, srcReg = valReg, dstReg = argReg}
            emit $ IR.If {condition = IR.NodeTypeExists irTag, srcReg = valReg, instructions = altInstructions}

          LitPat lit -> do
            altInstructions <- codeGenAlt $ do
              -- restrict scrutinee to alternative's domain
              flip (maybe (pure ())) scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                emit $ IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.SimpleTypeExists (litToSimpleType lit)
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }
            -- QUESTION: Redundant IF. Just for consistency?
            emit $ IR.If {condition = IR.SimpleTypeExists (litToSimpleType lit), srcReg = valReg, instructions = altInstructions}

          DefaultPat -> do
            tags <- Set.fromList <$> sequence [getTag tag | A (NodePat tag _) _ <- alts]
            altInstructions <- codeGenAlt $ do
              -- restrict scrutinee to alternative's domain
              flip (maybe (pure ())) scrutRegMapping $ \(name, _) -> do
                altScrutReg <- newReg
                addReg name altScrutReg
                emit $ IR.Project
                  { srcSelector = IR.ConditionAsSelector $ IR.NotIn tags
                  , srcReg = valReg
                  , dstReg = altScrutReg
                  }
            -- QUESTION: Redundant IF. Just for consistency?
            emit $ IR.If {condition = IR.NotIn tags, srcReg = valReg, instructions = altInstructions}

          _ -> throwE $ "HPT does not support the following case pattern: " ++ show cpat

      -- restore scrutinee register mapping
      maybe (pure ()) (uncurry addReg) scrutRegMapping

      pure $ R caseResultReg

    AltF cpat exp -> pure $ A cpat exp

    SAppF name args -> do -- copy args to definition's variables ; read function result register
      (funResultReg, funArgRegs) <- getOrAddFunRegs name $ length args
      valRegs <- mapM codeGenVal args
      zipWithM (\src dst -> emit $ IR.Move {srcReg = src, dstReg = dst}) valRegs funArgRegs
      -- HINT: handle primop here because it does not have definition
      when (isPrimName name) $ codeGenPrimOp name funResultReg funArgRegs
      pure $ R funResultReg

    SReturnF val -> R <$> codeGenVal val

    SStoreF val -> do
      loc <- newMem
      r <- newReg
      valReg <- codeGenVal val
      emit $ IR.Store {srcReg = valReg, address = loc}
      emit $ IR.Set {dstReg = r, constant = IR.CHeapLocation loc}
      pure $ R r

    SFetchIF name maybeIndex -> case maybeIndex of
      Just {} -> throwE "HPT codegen does not support indexed fetch"
      Nothing -> do
        addressReg <- getReg name
        r <- newReg
        emit $ IR.Fetch {addressReg = addressReg, dstReg = r}
        pure $ R r

    SUpdateF name val -> do
      addressReg <- getReg name
      valReg <- codeGenVal val
      emit $ IR.Update {srcReg = valReg, addressReg = addressReg}
      pure Z

    SBlockF exp -> exp

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
