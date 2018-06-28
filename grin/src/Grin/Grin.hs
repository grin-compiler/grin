{-# LANGUAGE FlexibleInstances, DeriveFunctor, RankNTypes, LambdaCase #-}
module Grin.Grin
  ( module Grin.Grin
  , module Grin.Syntax
  ) where

import Data.Functor.Foldable as Foldable
import Debug.Trace (trace)
import Lens.Micro.Platform
import Data.Maybe

import Grin.Syntax

isSimpleExp :: Exp -> Bool
isSimpleExp = \case
  SApp    _ _ -> True
  SReturn _   -> True
  SStore  _   -> True
  SFetchI _ _ -> True
  SUpdate _ _ -> True
  SBlock  _   -> True
  _           -> False

isBasicValue :: Val -> Bool
isBasicValue = \case
  ValTag _ -> True
  Unit     -> True
  Lit _    -> True
  _        -> False

class FoldNames n where
  foldNames :: (Monoid m) => (Name -> m) -> n -> m

instance FoldNames Val where
  foldNames f = \case
    ConstTagNode  _tag vals -> mconcat $ foldNames f <$> vals
    VarTagNode    name vals -> mconcat $ (f name) : (foldNames f <$> vals)
    ValTag        _tag      -> mempty
    Unit                    -> mempty
    -- simple val
    Lit lit                 -> mempty
    Var name                -> f name

match :: Traversal' a b -> a -> Bool
match t x = isJust $ x ^? t

isLit :: Val -> Bool
isLit = match _Lit

_Lit :: Traversal' Val Lit
_Lit f (Lit l) = Lit <$> f l
_Lit _ rest    = pure rest

_Var :: Traversal' Val Name
_Var f (Var name) = Var <$> f name
_Var _ rest       = pure rest

isBasicCPat :: CPat -> Bool
isBasicCPat = \case
  TagPat _ -> True
  LitPat _ -> True
  _        -> False

instance FoldNames CPat where
  foldNames f = \case
    NodePat _ names -> mconcat (map f names)
    TagPat _    -> mempty
    LitPat _    -> mempty
    DefaultPat  -> mempty

data NamesInExpF e a = NamesInExpF
  { namesExp   :: ExpF e
  , namesNameF :: Name -> a
  } deriving (Functor)

instance Foldable (NamesInExpF Exp) where
  foldr f b (NamesInExpF expf fn) = case expf of
    ProgramF  _            -> b
    DefF      name names _ -> foldr f b $ map fn (name:names)
    -- Exp
    EBindF    se lPat _  -> foldr f (foldr f b (NamesInExpF (project se) fn)) $ namesInVal lPat
    ECaseF    val _      -> foldr f b $ namesInVal val
    -- Simple Expr

    -- Does not collect function names in application
    SAppF     _name simpleVals -> foldr f b $ (map fn $ concatMap (foldNames list) simpleVals)
    SReturnF  val -> foldr f b $ namesInVal val
    SStoreF   val -> foldr f b $ namesInVal val
    SFetchIF  name mpos -> f (fn name) b
    SUpdateF  name val  -> foldr f b (fn name : namesInVal val)
    SBlockF   _ -> b
    -- Alt
    AltF cPat _ -> foldr f b $ map fn $ foldNames list cPat
    where
      namesInVal = map fn . foldNames list
      list x = [x]

dCoAlg :: (a -> String) -> (a -> ExpF b) -> (a -> ExpF b)
dCoAlg dbg f = f . (\x -> trace (dbg x) x)

dAlg :: (b -> String) -> (ExpF a -> b) -> (ExpF a -> b)
dAlg dbg f = (\x -> trace (dbg x) x) . f

isConstant :: Val -> Bool
isConstant = cata $ \case
  ConstTagNodeF  tag params -> and params
  ValTagF        tag        -> True
  UnitF                     -> True
  LitF lit                  -> True
  _                         -> False

hasConstant :: Val -> Bool
hasConstant = cata $ \case
  ValTagF{} -> True
  UnitF     -> True
  LitF{}    -> True
  v         -> or v

isAllVar :: Val -> Bool
isAllVar = cata $ \case
  ConstTagNodeF _ params  -> and params
  VarF{}                  -> True
  _                       -> False
