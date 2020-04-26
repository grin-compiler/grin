{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, EmptyCase, TypeApplications, RankNTypes #-}
module Reducer.Interpreter.Definitional.Instance where

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

import Reducer.Interpreter.Definitional.Internal


-- * Definitional Interpreter


simpleValue :: Syntax.Lit -> SVal
simpleValue = \case
  Syntax.LInt64  i -> SInt64 i
  Syntax.LWord64 w -> SWord64 w
  Syntax.LFloat  f -> SFloat f
  Syntax.LBool   b -> SBool b
  Syntax.LChar   c -> SChar c
  Syntax.LString t -> SString t

instance (Show v, HeapInfo i, Applicative m, Monad m, MonadFail m, MonadIO m) => Interpreter (DefinitionalT m i e v) where
  type Val     (DefinitionalT m i e v) = Either DVal v
  type HeapVal (DefinitionalT m i e v) = Node
  type Addr    (DefinitionalT m i e v) = Loc
  type Expr    (DefinitionalT m i e v) = e

  value :: Syntax.Val -> DefinitionalT m i e v (Either DVal v)
  value = \case
    Syntax.ConstTagNode t ns -> do
      p  <- askEnv
      vs <- pure $ map (Env.lookup p) ns
      pure $ Left $ DNode $ Node t $ map
        (\case
          Left (DVal v) -> v
          other -> error $ "value " ++ show other)
        vs
    Syntax.Lit l -> pure $ Left $ DVal $ simpleValue l
    Syntax.Unit  -> pure $ Left DUnit
    Syntax.Var v -> error "Variable lookup is not supported."

  val2addr :: (Either DVal v) -> DefinitionalT m i e v Loc
  val2addr v@(Right{}) = error $ "val2addr " ++ show v
  val2addr (Left v) = case v of
    (DVal (SLoc l)) -> pure l
    other           -> error $ "val2addr " ++ show other

  addr2val :: Loc -> DefinitionalT m i e v (Either DVal v)
  addr2val = pure . Left . DVal . SLoc

  heapVal2val :: Node -> DefinitionalT m i e v (Either DVal v)
  heapVal2val = pure . Left . DNode

  val2heapVal :: (Either DVal v) -> DefinitionalT m i e v Node
  val2heapVal o@(Right{}) = error $ "val2heapVal: " ++ show o
  val2heapVal (Left v) = case v of
    DNode n -> pure n
    other   -> error $ "val2heapVal: " ++ show other

  unit :: DefinitionalT m i e v (Either DVal v)
  unit = pure $ Left DUnit

  bindPattern :: (Either DVal v) -> (Tag, [Name]) -> DefinitionalT m i e v [(Name, (Either DVal v))]
  bindPattern (Left (DNode (Node t0 vs))) (t1, ps)
    | t0 == t1  = pure (ps `zip` (Left . DVal <$> vs))
  bindPattern pattern match = error $ "bindPattern: " ++ show (pattern, match)

  askEnv :: (DefinitionalT m i e v) (Env (Either DVal v))
  askEnv = _defEnv <$> ask

  localEnv :: Env (Either DVal v) -> (DefinitionalT m i e v) (Either DVal v) -> (DefinitionalT m i e v) (Either DVal v)
  localEnv e = local (defEnv .~ e)

  isExternal :: Name -> (DefinitionalT m i e v) Bool
  isExternal funName = (Map.member funName . _defOps) <$> ask

  external :: Name -> [(Either DVal v)] -> (DefinitionalT m i e v) (Either DVal v)
  external funName params = DefinitionalT $ do
    op <- lift ((fromJust . Map.lookup funName . _defOps) <$> ask)
    lift (lift (op params))

  matchingVal :: (Either DVal v) -> [Fix (Syntax.ExpF :+: e)] -> DefinitionalT m i e v (Env (Either DVal v), Fix (Syntax.ExpF :+: e))
  matchingVal o@(Right{}) _ = error $ "matchingVal: " ++ show o
  matchingVal (Left v) alts = do
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

      calcMatchEnv :: DVal -> Fix (Syntax.ExpF :+: e) -> Env (Either DVal v)
      calcMatchEnv (DNode (Node t0 vs)) (Fix (Inl (Syntax.AltF (Syntax.NodePat t1 nps) n body)))
        | t0 == t1  = Env.inserts (nps `zip` (Left . DVal <$> vs)) $ Env.insert n (Left v) Env.empty
        | otherwise = error "mismatching branch"
      calcMatchEnv _ (Fix (Inl (Syntax.AltF _ n body))) = Env.insert n (Left v) Env.empty
      calcMatchEnv pat alt = error "calcMatchEnv"

  funCall :: Name -> [(Either DVal v)] -> DefinitionalT m i e v (Env (Either DVal v), Fix (Syntax.ExpF :+: e))
  funCall fn vs = do
    (Fix (Inl (Syntax.DefF _ fps body))) <- lookupFun fn
    let p = Env.inserts (fps `zip` vs) Env.empty
    pure (p, body)

  allocStore :: Name -> DefinitionalT m i e v (Either DVal v)
  allocStore _ = do
    (Store s) <- get
    let a = Loc $ Map.size s
    DefinitionalT $ modify (Store.insert a (HeapNode Nothing storeHeapInfo))
    addr2val a

  fetchStore :: (Either DVal v) -> DefinitionalT m i e v (Either DVal v)
  fetchStore l = do
    a <- val2addr l
    DefinitionalT $ modify (Store.modify a (\(HeapNode n i) -> HeapNode n (fetchHeapInfo i)))
    s <- get
    heapVal2val $ fromJust $ heapNode $ Store.lookup a s

  extStore :: (Either DVal v) -> (Either DVal v) -> DefinitionalT m i e v ()
  extStore l n = do
    a <- val2addr l
    v <- val2heapVal n
    DefinitionalT $ modify (Store.modify a (\(HeapNode _ i) -> HeapNode (Just v) (updateHeapInfo i)))

evalDefinitional
  :: (Monad m, Show v, HeapInfo i, MonadFail m, MonadIO m)
  => DefinitionalTContext e v i
  -> (forall a . e a -> DefinitionalT m i e v (Either DVal v))
  -> Map.Map Syntax.Name ([DVal] -> m DVal)
  -> Name
  -> Fix (Syntax.ExpF :+: e)
  -> m (Either DVal v, Store Loc (HeapNode i))
evalDefinitional ctx evalExt ops mainName exp
  = runDefinitionalT ctx ops exp (eval evalExt (Fix (Inl (Syntax.SAppF mainName []))))
  where
    convertPrimOp f args = liftIO $ fmap rtValToDVal $ f $ map dValToRtVal args

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
