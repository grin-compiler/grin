{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, EmptyCase #-}
module Reducer.Interpreter.Definitional where

import Control.Monad (forM_, when)
import Control.Monad.Fail
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Monad.Trans.Reader hiding (ask, local)
import Control.Monad.Trans.State hiding (state, get)
import Data.Int
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Word
import Data.Text (Text)
import Grin.ExtendedSyntax.Syntax (Name(..), Exp(..), Tag(..), Lit(..))
import qualified Grin.ExtendedSyntax.Syntax as Syntax
import Reducer.Interpreter.Base
import Lens.Micro.Platform
import Prelude hiding (fail)

import Reducer.Interpreter.Store (Store(..))
import qualified Reducer.Interpreter.Store as Store
import Reducer.Interpreter.Env (Env)
import qualified Reducer.Interpreter.Env as Env
import qualified Data.Map.Strict as Map
import qualified Grin.Syntax as SyntaxV1 (Exp, Name(..), Tag(..), Lit(..))
import Reducer.Base (RTVal(..), Statistics(..))
import Reducer.Pure (EvalPlugin(..))
import Grin.Statistics (Statistics(..))
import Transformations.ExtendedSyntax.Conversion (convertToNew, convert)
import Data.Functor.Foldable


-- * Definitional Interpreter

data SVal
  = SInt64  Int64
  | SWord64 Word64
  | SFloat  Float
  | SBool   Bool
  | SChar   Char
  | SString Text
  | SLoc    Loc
  deriving (Eq, Ord, Show)

simpleValue :: Syntax.Lit -> SVal
simpleValue = \case
  Syntax.LInt64  i -> SInt64 i
  Syntax.LWord64 w -> SWord64 w
  Syntax.LFloat  f -> SFloat f
  Syntax.LBool   b -> SBool b
  Syntax.LChar   c -> SChar c
  Syntax.LString t -> SString t

data Node = Node Tag [SVal]
  deriving (Eq, Ord, Show)

newtype Loc = Loc Int
  deriving (Eq, Ord, Show)

data DVal
  = DNode Node
  | DVal  SVal
  | DUnit
  deriving (Eq, Ord, Show)

data DefEnv m e v = DefEnv
  { _defFuns :: Map.Map Name (Fix (Syntax.ExpF :+: e))
  , _defOps  :: Map.Map Name ([v] -> m v)
  , _defEnv  :: Env v
  }

makeLenses ''DefEnv

data HeapNode = HeapNode { heapNode :: !Node , fetched :: !Int, updated :: !Int }

-- TODO: Use RWST
newtype DefinitionalT (m :: * -> *) (e :: * -> *) (a :: *) = DefinitionalT
  { definitionalT :: StateT (Store Loc HeapNode) (ReaderT (DefEnv m e DVal) m) a
  } deriving
      ( Functor
      , Applicative
      , Monad
      , MonadFail
      , MonadIO
      , MonadReader (DefEnv m e DVal)
      , MonadState (Store Loc HeapNode)
      )

runDefinitionalT :: (Monad m) => Exp -> Map.Map Syntax.Name ([DVal] -> m DVal) -> DefinitionalT m e a -> m a
runDefinitionalT prog ops n = runReaderT (evalStateT (definitionalT n) Store.empty) env
  where
    env = DefEnv (programToDefs prog) ops Env.empty

instance (Applicative m, Monad m, MonadFail m, MonadIO m) => Interpreter (DefinitionalT m e) where
  type Val     (DefinitionalT m e) = DVal
  type HeapVal (DefinitionalT m e) = Node
  type Addr    (DefinitionalT m e) = Loc
  type Expr    (DefinitionalT m e) = e

  value :: Syntax.Val -> DefinitionalT m e DVal
  value = \case
    Syntax.ConstTagNode t ns -> do
      p  <- askEnv
      vs <- pure $ map (Env.lookup p) ns
      pure $ DNode $ Node t $ map
        (\case
          DVal v -> v
          other -> error $ "value " ++ show other)
        vs
    Syntax.Lit l -> pure $ DVal $ simpleValue l
    Syntax.Unit  -> pure DUnit
    Syntax.Var v -> error "Variable lookup is not supported."

  val2addr :: DVal -> DefinitionalT m e Loc
  val2addr = \case
    (DVal (SLoc l)) -> pure l
    other           -> error $ "val2addr" ++ show other

  addr2val :: Loc -> DefinitionalT m e DVal
  addr2val = pure . DVal . SLoc

  heapVal2val :: Node -> DefinitionalT m e DVal
  heapVal2val = pure . DNode

  val2heapVal :: DVal -> DefinitionalT m e Node
  val2heapVal = \case
    DNode n -> pure n
    other   -> error $ "val2heapVal: " ++ show other

  unit :: DefinitionalT m e DVal
  unit = pure DUnit

  bindPattern :: DVal -> (Tag, [Name]) -> DefinitionalT m e [(Name, DVal)]
  bindPattern (DNode (Node t0 vs)) (t1, ps)
    | t0 == t1  = pure (ps `zip` (DVal <$> vs))
  bindPattern pattern match = error $ "bindPattern: " ++ show (pattern, match)

  askEnv :: (DefinitionalT m e) (Env DVal)
  askEnv = _defEnv <$> ask

  localEnv :: Env DVal -> (DefinitionalT m e) DVal -> (DefinitionalT m e) DVal
  localEnv e = local (defEnv .~ e)

  isExternal :: Name -> (DefinitionalT m e) Bool
  isExternal funName = (Map.member funName . _defOps) <$> ask

  external :: Name -> [DVal] -> (DefinitionalT m e) DVal
  external funName params = DefinitionalT $ do
    op <- lift ((fromJust . Map.lookup funName . _defOps) <$> ask)
    lift (lift (op params))

  matchingVal :: DVal -> [Fix (Syntax.ExpF :+: e)] -> DefinitionalT m e (Env DVal, Fix (Syntax.ExpF :+: e))
  matchingVal v alts = do
    let selectedAlt = head $ filter (\case { Fix (Inl (Syntax.AltF p n _b)) -> match v p ; _ -> False }) alts
    pure $ (calcMatchEnv v selectedAlt, selectedAlt)
    where
      match :: DVal -> Syntax.CPat -> Bool
      match DUnit                 p                       = error $ "matching failure:" ++ show (DUnit, p)
      match (DVal (SLoc l))       p                       = error $ "matching failure:" ++ show (l, p)
      match (DNode (Node t0 _p))  (Syntax.NodePat t1 _v)  = t0 == t1
      match (DVal l0)             (Syntax.LitPat l1)      = l0 == (simpleValue l1)
      match (DNode{})             Syntax.DefaultPat       = True
      match (DVal{})              Syntax.DefaultPat       = True
      match _                     _                       = False

      calcMatchEnv :: DVal -> Fix (Syntax.ExpF :+: e) -> Env DVal
      calcMatchEnv (DNode (Node t0 vs)) (Fix (Inl (Syntax.AltF (Syntax.NodePat t1 nps) n body)))
        | t0 == t1  = Env.inserts (nps `zip` (DVal <$> vs)) $ Env.insert n v Env.empty
        | otherwise = error "mismatching branch"
      calcMatchEnv _ (Fix (Inl (Syntax.AltF _ n body))) = Env.insert n v Env.empty
      calcMatchEnv pat alt = error "calcMatchEnv"

  funCall :: Name -> [DVal] -> DefinitionalT m e (Env DVal, Fix (Syntax.ExpF :+: e))
  funCall fn vs = do
    (Fix (Inl (Syntax.DefF _ fps body))) <- lookupFun fn
    let p = Env.inserts (fps `zip` vs) Env.empty
    pure (p, body)

  allocStore :: Name -> DefinitionalT m e DVal
  allocStore _ = do
    (Store s) <- get
    let a = Loc $ Map.size s
    addr2val a

  fetchStore :: DVal -> DefinitionalT m e DVal
  fetchStore l = do
    a <- val2addr l
    DefinitionalT $ modify (Store.modify a (\(HeapNode n f u) -> HeapNode n (succ f) u))
    s <- get
    heapVal2val $ heapNode $ Store.lookup a s

  extStore :: DVal -> DVal -> DefinitionalT m e ()
  extStore l n = do
    a <- val2addr l
    v <- val2heapVal n
    DefinitionalT $ modify (Store.modify a (\(HeapNode _ f u) -> HeapNode v f (succ u)))

lookupFun :: (Monad m) => Name -> (DefinitionalT m e) (Fix (Syntax.ExpF :+: e))
lookupFun funName = (fromMaybe (error $ "Missing:" ++ show funName) . Map.lookup funName . _defFuns) <$> ask


evalDefinitional :: (Monad m, MonadFail m, MonadIO m) => EvalPlugin -> Name -> Exp -> m DVal
evalDefinitional (EvalPlugin evalPrimOps) mainName prog = do
  let ops = Map.map convertPrimOp $ Map.mapKeys nameV1toV2 evalPrimOps
  runDefinitionalT prog ops (eval evalVoid (Fix (Inl (Syntax.SAppF mainName []))))
  where
    exts = Syntax.externals prog
    convertPrimOp f args = liftIO $ fmap rtValToDVal $ f $ map dValToRtVal args

    evalVoid :: Void a -> m DVal
    evalVoid = \case

nameV1toV2 :: SyntaxV1.Name -> Syntax.Name
nameV1toV2 = \case
  SyntaxV1.NM t -> NM t
  SyntaxV1.NI i -> NI i

svalToRTVal :: SVal -> RTVal
svalToRTVal = \case
  SInt64  int64     -> RT_Lit $ SyntaxV1.LInt64  int64
  SWord64 word64    -> RT_Lit $ SyntaxV1.LWord64 word64
  SFloat  float     -> RT_Lit $ SyntaxV1.LFloat  float
  SBool   bool      -> RT_Lit $ SyntaxV1.LBool   bool
  SChar   char      -> RT_Lit $ SyntaxV1.LChar   char
  SString text      -> RT_Lit $ SyntaxV1.LString text
  SLoc    (Loc loc) -> RT_Loc loc

dValToRtVal :: DVal -> RTVal
dValToRtVal = \case
  DNode (Node t vs) -> RT_ConstTagNode (convert t) (svalToRTVal <$> vs)
  DVal  sval        -> svalToRTVal sval
  DUnit             -> RT_Unit

rtValToSVal :: RTVal -> SVal
rtValToSVal = \case
  RT_Lit lit -> simpleValue $ convert lit
  RT_Loc loc -> SLoc (Loc loc)
  other -> error $ "rtValToSVal: unsupported rtval " ++ show other

rtValToDVal :: RTVal -> DVal
rtValToDVal = \case
  RT_ConstTagNode tag  vs -> DNode $ Node (convert tag) $ map rtValToSVal vs
  RT_Unit                 -> DUnit
  RT_Lit          lit     -> DVal $ simpleValue $ convert lit
  RT_Loc          int     -> DVal $ SLoc $ Loc int
  other -> error $ "rtValToDVal: unsupported" ++ show other

reduceFun :: EvalPlugin -> SyntaxV1.Exp -> SyntaxV1.Name -> IO RTVal
reduceFun plugin expV1 mainName = do
  let expV2 = convertToNew expV1
  dval <- evalDefinitional plugin (nameV1toV2 mainName) expV2
  pure $ dValToRtVal dval
