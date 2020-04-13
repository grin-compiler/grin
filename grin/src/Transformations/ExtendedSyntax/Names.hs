{-# LANGUAGE LambdaCase, RecordWildCards, FlexibleInstances #-}
module Transformations.ExtendedSyntax.Names where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Bifunctor (second)

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Text.Printf

import Control.Monad
import Control.Monad.State

import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Util

-- name monad

data NameEnv
  = NameEnv
  { namePool  :: Map Name Int
  , nameSet   :: Set Name
  }

instance Semigroup NameEnv where
  (NameEnv np1 ns1) <> (NameEnv np2 ns2) = NameEnv (np1 <> np2) (ns1 <> ns2)

instance Monoid NameEnv where
  mempty = NameEnv mempty mempty

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

deriveWildCard :: NameM Name
deriveWildCard = do
  (newWildCard, conflict) <- state $ \env@NameEnv{..} ->
    let wildcard = "_"
        idx = Map.findWithDefault 0 wildcard namePool
        new = packName $ printf "%s%d" wildcard idx
    in  ( (new, Set.member new nameSet)
        , env {namePool = Map.insert wildcard (succ idx) namePool, nameSet = Set.insert new nameSet}
        )
  if conflict
    then deriveWildCard
    else pure newWildCard

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

runEvalNameM :: NameM a -> a
runEvalNameM m = evalState m mempty

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
  SApp name args    -> SApp       <$> f name <*> mapM f args
  ECase scrut alts  -> ECase      <$> f scrut <*> pure alts
  SReturn val       -> SReturn    <$> mapNamesValM f val
  SStore var        -> SStore     <$> f var
  SFetch ptr        -> SFetch     <$> f ptr
  SUpdate ptr var   -> SUpdate    <$> f ptr <*> f var
  exp               -> pure exp

mapNameDefExpM :: Monad m => (Name -> m Name) -> Exp -> m Exp
mapNameDefExpM f = \case
  Def name args body          -> Def <$> f name <*> mapM f args <*> pure body
  EBind leftExp (VarPat var) rightExp -> do EBind leftExp <$> (VarPat <$> f var) <*> pure rightExp
  EBind leftExp (AsPat tag args var) rightExp -> EBind leftExp <$> (AsPat tag <$> mapM f args <*> f var) <*> pure rightExp
  Alt cpat n body             -> Alt <$> mapNamesCPatM f cpat <*> f n <*> pure body
  exp                         -> pure exp

mapNamesCPatM :: Monad m => (Name -> m Name) -> CPat -> m CPat
mapNamesCPatM f = \case
  NodePat tag args  -> NodePat tag <$> mapM f args
  cpat -> pure cpat

mapNamesValM :: Monad m => (Name -> m Name) -> Val -> m Val
mapNamesValM f = \case
  ConstTagNode tag args -> ConstTagNode tag <$> mapM f args
  Var name              -> Var <$> f name
  val                   -> pure val
