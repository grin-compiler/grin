{-# LANGUAGE FlexibleInstances, DeriveFunctor, RankNTypes, LambdaCase #-}
module Grin.Grin
  ( module Grin.Grin
  , module Grin.Syntax
  ) where

import Data.Functor.Foldable as Foldable
import Debug.Trace (trace)
import Lens.Micro.Platform
import Data.Maybe
import Data.Text (pack, unpack)
import Data.List (nub)

import Grin.Syntax
import Grin.TypeEnvDefs

class FoldNames n where
  foldNames :: (Monoid m) => (Name -> m) -> n -> m

instance FoldNames Val where
  foldNames f = \case
    ConstTagNode  _tag vals -> mconcat $ foldNames f <$> vals
    VarTagNode    name vals -> mconcat $ (f name) : (foldNames f <$> vals)
    ValTag        _tag      -> mempty
    Unit                    -> mempty
    Undefined _             -> mempty
    -- simple val
    Lit lit                 -> mempty
    Var name                -> f name

instance FoldNames CPat where
  foldNames f = \case
    NodePat _ names -> mconcat (map f names)
    TagPat _    -> mempty
    LitPat _    -> mempty
    DefaultPat  -> mempty

instance FoldNames n => FoldNames [n] where
  foldNames f = mconcat . map (foldNames f)

dCoAlg :: (a -> String) -> (a -> ExpF b) -> (a -> ExpF b)
dCoAlg dbg f = f . (\x -> trace (dbg x) x)

dAlg :: (b -> String) -> (ExpF a -> b) -> (ExpF a -> b)
dAlg dbg f = (\x -> trace (dbg x) x) . f

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

_CNode :: Traversal' Val (Tag, [Val])
_CNode f (ConstTagNode tag params) = uncurry ConstTagNode <$> f (tag, params)
_CNode _ rest = pure rest

_VarNode :: Traversal' Val (Name, [Val])
_VarNode f (VarTagNode name params) = uncurry VarTagNode <$> f (name, params)
_VarNode _ rest = pure rest

isBasicCPat :: CPat -> Bool
isBasicCPat = \case
  TagPat _ -> True
  LitPat _ -> True
  _        -> False

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

isBasicValue :: Val -> Bool
isBasicValue = \case
  ValTag _ -> True
  Unit     -> True
  Lit _    -> True
  _        -> False

isPrimitiveExp :: Exp -> Bool
isPrimitiveExp = \case
  SApp    _ _ -> True
  SReturn _   -> True
  SStore  _   -> True
  SFetchI _ _ -> True
  SUpdate _ _ -> True
  _           -> False

isSimpleExp :: Exp -> Bool
isSimpleExp e | isPrimitiveExp e = True
isSimpleExp e = case e of
  SBlock  _   -> True
  _           -> False

unpackName :: Name -> String
unpackName (NM name) = unpack name

packName :: String -> Name
packName = NM . pack

showTS :: Show a => a -> Name
showTS = packName . show

concatPrograms :: [Program] -> Program
concatPrograms prgs = Program (nub $ concat exts) (concat defs) where
  (exts, defs) = unzip [(e, d) | Program e d <- prgs]

-- indetifier rules for parser and pretty printer
allowedSpecial :: String
allowedSpecial = "._':!@-"

allowedInitial :: String
allowedInitial = "._" ++ ['a'..'z'] ++ ['A'..'Z']
