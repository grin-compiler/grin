{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Transformations.Util where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State
import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Foldable as Foldable

import Grin.Grin

{-
  HINT: Name usage in Exp
    - variable def
        names in CPat
        names in LPat
        arg names in Def

    - variable use
        names in Val
        names in FetchI and Update

    - function binder
        function name in Def

    - function reference
        function name in SApp
-}

foldNamesVal :: (Monoid m) => (Name -> m) -> Val -> m
foldNamesVal f = \case
  ConstTagNode tag vals -> mconcat $ map (foldNamesVal f) vals
  VarTagNode name vals  -> mconcat $ f name : map (foldNamesVal f) vals
  Var name              -> f name
  _                     -> mempty

foldNameUseExpF :: (Monoid m) => (Name -> m) -> ExpF a -> m
foldNameUseExpF f = \case
  ECaseF val _      -> foldNamesVal f val
  SAppF name vals   -> mconcat $ map (foldNamesVal f) vals
  SReturnF val      -> foldNamesVal f val
  SStoreF val       -> foldNamesVal f val
  SUpdateF name val -> mconcat $ [f name, foldNamesVal f val]
  SFetchIF name i   -> f name
  _                 -> mempty

foldNameDefExpF :: (Monoid m) => (Name -> m) -> ExpF a -> m
foldNameDefExpF f = \case
  DefF name args _  -> mconcat $ f name : map f args
  EBindF _ lpat _   -> foldNamesVal f lpat
  AltF cpat _       -> foldNamesCPat f cpat
  _                 -> mempty

foldNamesCPat :: Monoid m => (Name -> m) -> CPat -> m
foldNamesCPat f = \case
  NodePat _ args  -> mconcat $ map f args
  cpat            -> mempty

mapNamesCPat :: (Name -> Name) -> CPat -> CPat
mapNamesCPat f = \case
  NodePat tag args  -> NodePat tag (map f args)
  cpat              -> cpat

mapNamesVal :: (Name -> Name) -> Val -> Val
mapNamesVal f = \case
  ConstTagNode tag vals -> ConstTagNode tag (map (mapNamesVal f) vals)
  VarTagNode name vals  -> VarTagNode (f name) (map (mapNamesVal f) vals)
  Var name              -> Var $ f name
  val                   -> val

mapValVal :: (Val -> Val) -> Val -> Val
mapValVal f val = case f val of
  ConstTagNode tag vals -> ConstTagNode tag (map (mapValVal f) vals)
  VarTagNode name vals  -> VarTagNode name (map (mapValVal f) vals)
  val                   -> val

mapValsExp :: (Val -> Val) -> Exp -> Exp
mapValsExp f = \case
  ECase val alts    -> ECase (f val) alts
  SApp name vals    -> SApp name (map f vals)
  SReturn val       -> SReturn $ f val
  SStore val        -> SStore $ f val
  SUpdate name val  -> SUpdate name $ f val
  exp               -> exp

mapNameUseExp :: (Name -> Name) -> Exp -> Exp
mapNameUseExp f = \case
  SFetchI name i    -> SFetchI (f name) i
  SUpdate name val  -> SUpdate (f name) $ mapNamesVal f val
  exp               -> mapValsExp (mapNamesVal f) exp

subst :: Ord a => Map a a -> a -> a
subst env x = Map.findWithDefault x x env

-- variable reference substitution (non recursive)
substVarRefExp :: Map Name Name -> Exp -> Exp
substVarRefExp env = mapNameUseExp (subst env)

-- val name substitution (non recursive)
substNamesVal :: Map Name Name -> Val -> Val
substNamesVal env = mapNamesVal (subst env)

-- val name substitution (non recursive)
substValsVal :: Map Val Val -> Val -> Val
substValsVal env = mapValVal (subst env)

-- val substitution (non recursive)
substVals :: Map Val Val -> Exp -> Exp
substVals env = mapValsExp (mapValVal $ subst env)

cpatToLPat :: CPat -> LPat
cpatToLPat = \case
  NodePat tag args  -> ConstTagNode tag (map Var args)
  LitPat  lit       -> Lit lit
  TagPat  tag       -> ValTag tag
  DefaultPat        -> Unit

-- monadic recursion schemes
--  see: https://jtobin.io/monadic-recursion-schemes

cataM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t a -> m a) -> t ->  m a
cataM alg = c where
    c = alg <=< traverse c . project

anaM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t a)) -> a -> m t
anaM coalg = a where
  a = (pure . embed) <=< traverse a <=< coalg

paraM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t (t, a) -> m a) -> t -> m a
paraM alg = p where
  p   = alg <=< traverse f . project
  f t = liftM2 (,) (pure t) (p t)

apoM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t (Either t a))) -> a -> m t
apoM coalg = a where
  a = (pure . embed) <=< traverse f <=< coalg
  f = either pure a

hyloM
  :: (Monad m, Traversable t)
  => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h
  where h = alg <=< traverse h <=< coalg

histoM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t (Cofree (Base t) a) -> m a) -> t -> m a
histoM h = pure . extract <=< worker where
  worker = f <=< traverse worker . project
  f x = (:<) <$> h x <*> pure x

-- misc

skipUnit :: ExpF Exp -> Exp
skipUnit = \case
  EBindF (SReturn Unit) Unit rightExp -> rightExp
  exp -> embed exp

newtype TagInfo = TagInfo { _tagArityMap :: Map.Map Tag Int }
  deriving (Eq, Show)

updateTagInfo :: Tag -> Int -> TagInfo -> TagInfo
updateTagInfo t n ti@(TagInfo m) =
  case Map.lookup t m of
    Just arity | arity < n -> TagInfo $ Map.insert t n m
    Nothing                -> TagInfo $ Map.insert t n m
    _                      -> ti

collectTagInfo :: Exp -> TagInfo
collectTagInfo = flip execState (TagInfo Map.empty) . cataM alg
  where
    alg :: ExpF () -> State TagInfo ()
    alg = \case
      ECaseF val _   -> goVal val
      SReturnF val   -> goVal val
      SAppF _ vals   -> mapM_ goVal vals
      SStoreF val    -> goVal val
      SUpdateF _ val -> goVal val
      AltF cpat _    -> goCPat cpat
      _              -> pure ()

    goVal :: Val -> State TagInfo ()
    goVal (ConstTagNode t args) = modify $ updateTagInfo t (length args)
    goVal _ = pure ()

    goCPat :: CPat -> State TagInfo ()
    goCPat (NodePat t args) = modify $ updateTagInfo t (length args)
    goCPat _ = pure ()
