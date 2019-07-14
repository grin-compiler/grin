{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleInstances, InstanceSigs #-}
module Transformations.Names where

import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad
import Control.Monad.State

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Grin.Grin
import Transformations.Util
import Data.Bifunctor (second)

-- name monad

data NameEnv
  = NameEnv
  { namePool  :: Map Name Int
  , nameSet   :: Set Name
  }

type NameM = State NameEnv

mkNameEnv :: Exp -> NameEnv
mkNameEnv exp = NameEnv mempty (cata folder exp) where
  folder e = foldNameDefExpF (const Set.singleton) e `mappend` Data.Foldable.fold e

deriveNewName :: Name -> NameM Name
deriveNewName name = do
  (newName, conflict) <- state $ \env@NameEnv{..} ->
    let idx = Map.findWithDefault 0 name namePool
        new = packName $ printf "%s.%d" name idx
    in  ( (new, Set.member new nameSet)
        , env {namePool = Map.insert name (succ idx) namePool, nameSet = Set.insert new nameSet}
        )
  if conflict
    then deriveNewName name
    else pure newName

boolTF :: a -> a -> Bool -> a
boolTF true false x = if x then true else false

--TODO: this should be put into a Piple.Definitions module
data ExpChanges
  = NoChange
  | NewNames
  -- only relevant heap operations
  -- (e.g.: deleting a dead case alternative should not trigger this)
  | DeletedHeapOperation
  deriving (Eq, Show)

evalNameM :: Exp -> NameM a -> (a, ExpChanges)
evalNameM e m = second (boolTF NoChange NewNames . Map.null . namePool) $ runState m (mkNameEnv e)

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

instance MapName CPat where
  mapNameM :: Monad m => (Name -> m Name) -> CPat -> m CPat
  mapNameM f = \case
    NodePat tag args  -> NodePat tag <$> mapM f args
    cpat -> pure cpat

instance MapName Val where
  mapNameM :: Monad m => (Name -> m Name) -> Val -> m Val
  mapNameM f = \case
    ConstTagNode tag args -> ConstTagNode tag <$> mapM f args
    VarTagNode name args  -> VarTagNode <$> f name <*> mapM f args
    Var name              -> Var <$> f name
    val                   -> pure val

instance MapName BPat where
  mapNameM :: Monad m => (Name -> m Name) -> BPat -> m BPat
  mapNameM f = \case
    VarPat v -> VarPat <$> f v
    AsPat var val -> AsPat <$> f var <*> mapNameM f val

-- QUESTION: monadic lens (over)?
instance MapName AppName where
  mapNameM :: Monad m => (Name -> m Name) -> AppName -> m AppName
  mapNameM f (Ext name) = Ext <$> f name
  mapNameM f (Fun name) = Fun <$> f name

-- QUESTION: empty instances which are never used
-- instance MapName (UseSite Exp)
-- instance MapName (DefSite Exp)

-- instance MapVal (UseSite Exp)
-- instance MapVal (DefSite Exp)
-- instance MapVal Val

mapNameUseExpM :: Monad m => (Name -> m Name) -> Exp -> m Exp
mapNameUseExpM f = \case
  SApp name args    -> SApp       <$> mapNameM f name <*> mapM f args
  ECase scrut alts  -> ECase      <$> f scrut <*> pure alts
  SReturn val       -> SReturn    <$> mapNameM f val
  SStore var        -> SStore     <$> f var
  SFetchI var i    -> SFetchI     <$> f var <*> pure i
  SUpdate ptr var  -> SUpdate     <$> f ptr <*> f var
  exp               -> pure exp

mapNameDefExpM :: Monad m => (Name -> m Name) -> Exp -> m Exp
mapNameDefExpM f = \case
  Def name args body          -> Def <$> f name <*> mapM f args <*> pure body
  EBind leftExp bPat rightExp -> EBind leftExp <$> mapNameM f bPat <*> pure rightExp
  Alt cpat body               -> Alt <$> mapNameM f cpat <*> pure body
  exp                         -> pure exp
