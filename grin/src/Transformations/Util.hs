{-# LANGUAGE LambdaCase, TupleSections, FlexibleContexts #-}
module Transformations.Util where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Data.Functor.Foldable as Foldable

import Grin

{-
  HINT: Name usage in Exp
    - variable binder
        names in CPat
        names in LPat
        arg names in Def

    - variable reference
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

foldVarRefExpF :: (Monoid m) => (Name -> m) -> ExpF a -> m
foldVarRefExpF f = \case
  ECaseF val _      -> foldNamesVal f val
  SAppF name vals   -> mconcat $ map (foldNamesVal f) vals
  SReturnF val      -> foldNamesVal f val
  SStoreF val       -> foldNamesVal f val
  SUpdateF name val -> mconcat $ [f name, foldNamesVal f val]
  SFetchIF name i   -> f name
  _                 -> mempty

mapNamesCPat :: (Name -> Name) -> CPat -> CPat
mapNamesCPat f = \case
  NodePat tag args  -> NodePat tag (map f args)
  LitPat  lit       -> LitPat lit
  TagPat  tag       -> TagPat tag

mapNamesVal :: (Name -> Name) -> Val -> Val
mapNamesVal f = \case
  ConstTagNode tag vals -> ConstTagNode tag (map (mapNamesVal f) vals)
  VarTagNode name vals  -> VarTagNode (f name) (map (mapNamesVal f) vals)
  Var name              -> Var $ f name
  val                   -> val

mapValsExp :: (Val -> Val) -> Exp -> Exp
mapValsExp f = \case
  ECase val alts    -> ECase (f val) alts
  SApp name vals    -> SApp name (map f vals)
  SReturn val       -> SReturn $ f val
  SStore val        -> SStore $ f val
  SUpdate name val  -> SUpdate name $ f val
  exp               -> exp

mapVarRefExp :: (Name -> Name) -> Exp -> Exp
mapVarRefExp f = \case
  SFetchI name i    -> SFetchI (f name) i
  SUpdate name val  -> SUpdate (f name) $ mapNamesVal f val
  exp               -> mapValsExp (mapNamesVal f) exp

mapVarBindExp :: (Name -> Name) -> Exp -> Exp
mapVarBindExp f = \case
  EBind leftExp lpat rightExp -> EBind leftExp (mapNamesVal f lpat) rightExp
  Alt cpat body               -> Alt (mapNamesCPat f cpat) body
  exp                         -> exp

subst :: Ord a => Map a a -> a -> a
subst env x = Map.findWithDefault x x env

-- variable reference substitution (non recursive)
substVarRefExp :: Map Name Name -> Exp -> Exp
substVarRefExp env = mapVarRefExp (subst env)

-- val substitution (non recursive)
substVals :: Map Val Val -> Exp -> Exp
substVals env = mapValsExp (subst env)

cpatToLPat :: CPat -> LPat
cpatToLPat = \case
  NodePat tag args  -> ConstTagNode tag (map Var args)
  LitPat  lit       -> Lit lit
  TagPat  tag       -> ValTag tag

-- monadic recursion schemes
--  see: https://jtobin.io/monadic-recursion-schemes

anaM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t a)) -> a -> m t
anaM coalg = a where
  a = (return . embed) <=< traverse a <=< coalg

paraM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t (t, a) -> m a) -> t -> m a
paraM alg = p where
  p   = alg <=< traverse f . project
  f t = liftM2 (,) (return t) (p t)

apoM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t (Either t a))) -> a -> m t
apoM coalg = a where
  a = (return . embed) <=< traverse f <=< coalg
  f = either return a

hyloM
  :: (Monad m, Traversable t)
  => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h
  where h = alg <=< traverse h <=< coalg

-- misc

skipUnit :: ExpF Exp -> Exp
skipUnit = \case
  EBindF (SReturn Unit) Unit rightExp -> rightExp
  exp -> embed exp
