{-# LANGUAGE LambdaCase, RecordWildCards, RankNTypes #-}
module AbstractInterpretation.CodeGen where

import Data.Int
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
emit inst = modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {_absInstructions = inst : _absInstructions}

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
  funMap <- getsDfi _absFunctionArgMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      resReg <- newReg
      argRegs <- replicateM arity newReg
      let funRegs = (resReg, argRegs)
      modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {_absFunctionArgMap = Map.insert name funRegs _absFunctionArgMap}
      pure funRegs

newReg :: HasDataFlowInfo s => CG s IR.Reg
newReg = stateDfi $ \s@AbstractProgram{..} -> (IR.Reg _absRegisterCounter, s {_absRegisterCounter = succ _absRegisterCounter})

newMem :: HasDataFlowInfo s => CG s IR.Mem
newMem = stateDfi $ \s@AbstractProgram{..} -> (IR.Mem _absMemoryCounter, s {_absMemoryCounter = succ _absMemoryCounter})

addReg :: HasDataFlowInfo s => Name -> IR.Reg -> CG s ()
addReg name reg = modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {_absRegisterMap = Map.insert name reg _absRegisterMap}

getReg :: HasDataFlowInfo s => Name -> CG s IR.Reg
getReg name = do
  regMap <- getsDfi _absRegisterMap
  case Map.lookup name regMap of
    Nothing   -> throwE $ "unknown variable " ++ unpackName name
    Just reg  -> pure reg

getTag :: HasDataFlowInfo s => Tag -> CG s IR.Tag
getTag tag = do
  tagMap <- getsDfi _absTagMap
  case Bimap.lookup tag tagMap of
    Just t  -> pure t
    Nothing -> do
      let t = IR.Tag . fromIntegral $ Bimap.size tagMap
      modify' $ modifyInfo $ \s -> s {_absTagMap = Bimap.insert tag t tagMap}
      pure t

-- <<<<<<< HEAD

{- =======
unitType :: IR.SimpleType
unitType = -1

litToSimpleType :: Lit -> IR.SimpleType
litToSimpleType = \case
  LInt64  {}  -> -2
  LWord64 {}  -> -3
  LFloat  {}  -> -4
  LBool   {}  -> -5
  LString {}  -> -6
  LChar   {}  -> -7

codeGenBlock :: CG () -> CG [IR.Instruction]
>>>>>>> andorp/idris -}
codeGenBlock :: HasDataFlowInfo s => CG s a -> CG s (a,[IR.Instruction])
codeGenBlock genM = do
  instructions <- stateDfi $ \s@AbstractProgram{..} -> (_absInstructions, s {_absInstructions = []})
  ret <- genM
  blockInstructions <- stateDfi $ \s@AbstractProgram{..} -> (reverse _absInstructions, s {_absInstructions = instructions})
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
    mapM_ (restore reg) mName

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
  T_String              -> newRegWithSimpleType (-6)
  T_Char                -> newRegWithSimpleType (-7)
  T_UnspecifiedLocation -> newRegWithSimpleType (-8)
  T_Location locs -> do
    r <- newReg
    let locs' = map fromIntegral locs
    mapM_ (`extendSimpleType` r) locs'
    pure r
-- <<<<<<< HEAD
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

-- Generate a node type from type information,
-- but preserve the first field for tag information.
codeGenTaggedNodeType :: HasDataFlowInfo s =>
                         Tag -> Vector SimpleType -> CG s IR.Reg
codeGenTaggedNodeType tag ts = do
  let ts' = Vec.toList ts
  r <- newReg
  irTag <- getTag tag
  argRegs <- mapM codeGenSimpleType ts'
  emit IR.Set {dstReg = r, constant = IR.CNodeType irTag (length argRegs + 1)}
  forM_ (zip [1..] argRegs) $ \(idx, argReg) ->
    emit IR.Extend {srcReg = argReg, dstSelector = IR.NodeItem irTag idx, dstReg = r}
  pure r

codeGenType :: HasDataFlowInfo s =>
               (SimpleType -> CG s IR.Reg) ->
               (NodeSet -> CG s IR.Reg) ->
               Type -> CG s IR.Reg
codeGenType cgSimpleTy cgNodeTy = \case
  T_SimpleType t -> cgSimpleTy t
  T_NodeSet   ns -> cgNodeTy ns

isPointer :: IR.Predicate
isPointer = IR.ValueIn (IR.Range 0 (maxBound :: Int32))

isNotPointer :: IR.Predicate
isNotPointer = IR.ValueIn (IR.Range (minBound :: Int32) 0)

-- For simple types, copies only pointer information
-- For nodes, copies the structure and the pointer information in the fields
copyStructureWithPtrInfo :: IR.Reg -> IR.Reg -> IR.Instruction
copyStructureWithPtrInfo srcReg dstReg = IR.ConditionalMove
  { srcReg    = srcReg
  , predicate = isPointer
  , dstReg    = dstReg
  }
{-
=======
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
      string = litToSimpleType $ LString ""
      char  = litToSimpleType $ LChar ' '

  case name of
    "_prim_int_print" -> op [int] unit
    "_prim_string_print" -> op [string] unit
    "_prim_read_string" -> op [] string

    -- String
    "_prim_string_concat"  -> op [string, string] string
    "_prim_string_reverse" -> op [string] string
    "_prim_string_eq"      -> op [string, string] bool
    "_prim_string_head"    -> op [string] int
    "_prim_string_tail"    -> op [string] string
    "_prim_string_cons"    -> op [int, string] string
    "_prim_string_len"     -> op [string] int

    -- Conversion
    "_prim_int_str"      -> op [int] string
    "_prim_int_float"    -> op [int] float
    "_prim_float_string" -> op [float] string
    "_prim_char_int"     -> op [char] int
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
    -- FFI - TODO: Handle FFI appropiatey
    "_prim_ffi_file_eof" -> op [int] int
    unknown           -> error $ "codeGenPrimOp unknown prim-op: " ++ unpackName unknown

codeGenPhases :: CG IR.Reg -> [IR.Reg -> Exp -> CG ()] -> Exp -> Either String HPTProgram
codeGenPhases init phases e = (\(a,s) -> s<$a) . flip runState IR.emptyHPTProgram . runExceptT $ do
  sharingReg <- init
  mapM_ (\phase -> phase sharingReg e) phases
  modify' $ \s -> s { hptSharingReg = Just sharingReg }


hptCodeGen :: IR.Reg -> Exp -> CG ()
hptCodeGen _sharingReg = void . cata folder where
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

      -- save scruinee register mapping
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

>>>>>>> andorp/idris -}
