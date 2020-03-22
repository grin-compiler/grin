{-# LANGUAGE FlexibleInstances, DeriveFunctor, RankNTypes, LambdaCase #-}
module Grin.ExtendedSyntax.Grin
  ( module Grin.ExtendedSyntax.Grin
  , module Grin.ExtendedSyntax.Syntax
  ) where

import Data.Functor.Foldable as Foldable
import Debug.Trace (trace)
import Lens.Micro.Platform
import Data.Maybe
import Data.Text (pack, unpack)
import Data.List (nub)

import Grin.ExtendedSyntax.Syntax
import Grin.ExtendedSyntax.TypeEnvDefs

class FoldNames n where
  foldNames :: (Monoid m) => (Name -> m) -> n -> m

instance FoldNames Val where
  foldNames f = \case
    ConstTagNode  _tag vals -> foldMap f vals
    Var name                -> f name
    _                       -> mempty

instance FoldNames BPat where
  foldNames f = \case
    VarPat v     -> f v
    AsPat t vs v -> f v <> foldNames f (ConstTagNode t vs)


instance FoldNames CPat where
  foldNames f = \case
    NodePat _ names -> foldMap f names
    LitPat _        -> mempty
    DefaultPat      -> mempty

instance FoldNames n => FoldNames [n] where
  foldNames f = foldMap (foldNames f)

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

-- NOTE: This will return NM "" for everyrhing that is not a Var
_Var :: Traversal' Val Name
_Var f (Var name) = Var <$> f name
_Var _ rest       = pure rest

_CNode :: Traversal' Val (Tag, [Name])
_CNode f (ConstTagNode tag params) = uncurry ConstTagNode <$> f (tag, params)
_CNode _ rest = pure rest

isBasicCPat :: CPat -> Bool
isBasicCPat = \case
  LitPat _ -> True
  _        -> False

isBasicValue :: Val -> Bool
isBasicValue Unit{}   = True
isBasicValue Lit{}    = True
isBasicValue _'       = True

isPrimitiveExp :: Exp -> Bool
isPrimitiveExp = \case
  SApp    _ _ -> True
  SReturn _   -> True
  SStore  _   -> True
  SFetch  _   -> True
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

-- NOTE: @ is no longer an allowed special (due to as-patterns)
-- indetifier rules for parser and pretty printer
allowedSpecial :: String
allowedSpecial = "._':!-"

-- QUESTION: Should upper-case letters be allowed?
allowedInitial :: String
allowedInitial = "._" ++ ['a'..'z'] ++ ['A'..'Z']
