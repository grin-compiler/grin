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

subst :: Ord a => Map a a -> a -> a
subst env x = Map.findWithDefault x x env

-- variable reference substitution (non recursive)
substVarRefExp :: Map Name Name -> Exp -> Exp
substVarRefExp env = mapVarRefExp (subst env)

-- val substitution (non recursive)
substVals :: Map Val Val -> Exp -> Exp
substVals env = mapValsExp (subst env)

-- monadic recursion schemes

anaM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t a)) -> a -> m t
anaM coalg = a where
  a = (return . embed) <=< traverse a <=< coalg
