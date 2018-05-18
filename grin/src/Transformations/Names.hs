{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleInstances #-}
module Transformations.Names where

import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State

import Data.Functor.Foldable as Foldable
import Grin

import Transformations.Util

-- name monad

data NameEnv
  = NameEnv
  { namePool  :: Map Name Int
  }

type NameM = State NameEnv

mkNameEnv :: Exp -> NameEnv
mkNameEnv = undefined

deriveNewName :: Name -> NameM Name
deriveNewName name = state $ \env@NameEnv{..} ->
  let idx = Map.findWithDefault 0 name namePool
  in (printf "%s.%d" name idx, env {namePool = Map.insert name (succ idx) namePool})

evalNameM :: NameM a -> a
evalNameM m = evalState m (NameEnv mempty)

-- refresh names

type FreshM = StateT (Map Name Name) NameM

refreshNames :: Map Name Name -> Exp -> NameM (Exp, Map Name Name)
refreshNames nameMap e = runStateT (anaM builder e) nameMap where

  builder :: Exp -> FreshM (ExpF Exp)
  builder = fmap project . mapNameDefExpM defName <=< mapNameUseExpM useName

  defName :: Name -> FreshM Name
  defName n = do
    new <- lift $ deriveNewName n
    modify $ Map.insert n new
    pure new

  useName :: Name -> FreshM Name
  useName n = Map.findWithDefault n n <$> get

-- map names

newtype UseSite a = UseSite a
newtype DefSite a = DefSite a

class MapName a where
  mapNameM :: Monad m => (Name -> m Name) -> a -> m a

class MapVal a where
  mapValM :: Monad m => (Val -> m Val) -> a -> m a

instance MapName (UseSite Exp)
instance MapName (DefSite Exp)
instance MapName CPat
instance MapName Val

instance MapVal (UseSite Exp)
instance MapVal (DefSite Exp)
instance MapVal Val

mapNameUseExpM :: Monad m => (Name -> m Name) -> Exp -> m Exp
mapNameUseExpM f = \case
  SApp name vals    -> SApp name  <$> mapM (mapNamesValM f) vals
  ECase val alts    -> ECase      <$> mapNamesValM f val <*> pure alts
  SReturn val       -> SReturn    <$> mapNamesValM f val
  SStore val        -> SStore     <$> mapNamesValM f val
  SFetchI name i    -> SFetchI    <$> f name <*> pure i
  SUpdate name val  -> SUpdate    <$> f name <*> mapNamesValM f val
  exp               -> pure exp

mapNameDefExpM :: Monad m => (Name -> m Name) -> Exp -> m Exp
mapNameDefExpM f = \case
  Def name args body          -> Def name <$> mapM f args <*> pure body
  EBind leftExp lpat rightExp -> EBind leftExp <$> mapNamesValM f lpat <*> pure rightExp
  Alt cpat body               -> Alt <$> mapNamesCPatM f cpat <*> pure body
  exp                         -> pure exp

mapNamesCPatM :: Monad m => (Name -> m Name) -> CPat -> m CPat
mapNamesCPatM f = \case
  NodePat tag args  -> NodePat tag <$> mapM f args
  cpat -> pure cpat

mapNamesValM :: Monad m => (Name -> m Name) -> Val -> m Val
mapNamesValM f = \case
  ConstTagNode tag vals -> ConstTagNode tag <$> mapM (mapNamesValM f) vals
  VarTagNode name vals  -> VarTagNode <$> f name <*> mapM (mapNamesValM f) vals
  Var name              -> Var <$> f name
  val                   -> pure val
