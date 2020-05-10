{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Reducer.Interpreter.Definitional.Cib where

import Data.Maybe (mapMaybe)
import Grin.ExtendedSyntax.Syntax (Name(..))
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (gets, modify)
import Reducer.Interpreter.Base
import Reducer.Interpreter.Definitional.Internal
import Reducer.Interpreter.Definitional.Instance
import Data.Functor.Foldable (Fix)
import Data.Functor.Sum

import qualified Grin.ExtendedSyntax.Syntax as Syntax
import qualified Reducer.Interpreter.Store as Store
import qualified Reducer.Interpreter.Env as Env
import qualified Data.Set as Set
import qualified Data.Map as Map


-- * Expression

data CibF a
  = IncF Name
  | DecF Name
  | ResetF Name
  | ReuseF Name Name

instance Functor CibF where
  fmap _ = \case
    IncF    n   -> IncF   n
    DecF    n   -> DecF   n
    ResetF  n   -> ResetF n
    ReuseF  n m -> ReuseF n m

-- * Reference counter

data RefCounter = RefCounter { referenced :: !Int }
  deriving Show

instance HeapInfo RefCounter where
  storeHeapInfo    = RefCounter 1
  fetchHeapInfo  x = x
  updateHeapInfo x = x

incRefCntr :: RefCounter -> RefCounter
incRefCntr (RefCounter c) = RefCounter (succ c)

decRefCntr :: RefCounter -> RefCounter
decRefCntr (RefCounter c) = RefCounter (pred c)

-- * Value

data CVal = Box deriving (Eq, Show)

-- * Semantics

evalCibF
  :: (Monad m, MonadFail m, MonadIO m)
  => CibF a
  -> DefinitionalT m RefCounter CibF CVal (Either DVal CVal)
evalCibF = \case
  IncF n -> do
    -- Increment the reference counters transitively
    a <- (flip Env.lookup n) <$> getEnv
    l <- valToAddr a
    modifyCounterInfo incRefCntr l
    pure $ Left DUnit
  DecF n -> do
    a <- (flip Env.lookup n) <$> getEnv
    l <- valToAddr a
    modifyCounterInfo decRefCntr l
    pure $ Left DUnit
  ResetF n -> do
    -- Returns the heap location if its reference counter zero, otherwise BOX
    -- which means we need to create an new location in reuse.
    a <- (flip Env.lookup n) <$> getEnv
    l <- valToAddr a
    (HeapNode _ (RefCounter c)) <- gets (Store.lookup l)
    pure $ case c of
      0 -> a
      _ -> Right Box
  ReuseF n m -> do
    -- Tries to reuse the given heap location, if it is BOX than a new location
    -- needs to be created.
    env <- getEnv
    let a = Env.lookup env n
    let v = Env.lookup env m
    nd <- valToNode v
    case a of
      Right Box -> do
        -- same as store
        l <- store n
        Reducer.Interpreter.Base.update l nd
        addrToVal l
      Left _ -> do
        -- same as update
        addr <- valToAddr a
        Reducer.Interpreter.Base.update addr nd
        l <- valToAddr a
        modify $ Store.modify l (\(HeapNode c _) -> HeapNode c storeHeapInfo)
        pure a

-- * Helper

modifyCounterInfo
  :: forall m
   . (Monad m, MonadFail m, MonadIO m)
  => (RefCounter -> RefCounter) -> Loc -> DefinitionalT m RefCounter CibF CVal ()
modifyCounterInfo f l = do
  modifyLoc l
  go (Set.singleton l)
  where
    modifyLoc :: Loc -> DefinitionalT m RefCounter CibF CVal ()
    modifyLoc x = do
      modify $ Store.modify x (\(HeapNode c r) -> HeapNode c (f r))

    pointsTo :: Loc -> DefinitionalT m RefCounter CibF CVal [Loc]
    pointsTo loc = do
      (HeapNode (Just (Node _ vals)) _) <- gets (Store.lookup loc)
      pure $ mapMaybe (\case { SLoc x -> Just x; _ -> Nothing }) vals

    newLocs :: Set.Set Loc -> DefinitionalT m RefCounter CibF CVal (Set.Set Loc)
    newLocs visited = do
      new <- mapM pointsTo $ Set.toList visited
      let newLocSet = Set.fromList $ concat new
      pure $ Set.difference newLocSet visited

    go visited = do
      new <- newLocs visited
      if Set.null new
        then pure ()
        else do
          mapM_ modifyLoc (Set.toList new)
          go (Set.union new visited)

-- * Runner

evalCib
  :: (Monad m, MonadFail m, MonadIO m)
  => Map.Map Syntax.Name ([DVal] -> m DVal)
  -> Syntax.Name
  -> Fix (Sum Syntax.ExpF CibF)
  -> m (Either DVal CVal, Store.Store Loc (HeapNode RefCounter))
evalCib ops = evalDefinitional
  (DefinitionalTContext @CibF @CVal @RefCounter)
  evalCibF
  ops
