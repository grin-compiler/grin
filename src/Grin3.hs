{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}
module Grin3 where

import Control.DeepSeq
import Data.Map (Map)
import GHC.Generics (Generic)

type Name = String


data AST a
  = AExp a
  | ASExp a
  | AAlt a
  deriving (Generic, NFData, Eq, Show, Functor)

data ExpF a
  = BindF  (AST a) LPat (AST a)
  | CaseF  Val [AST a]
  | SExpF  (AST a)
  deriving (Generic, NFData, Eq, Show, Functor)

data SimpleExpF a
  = AppF     Name [SimpleVal]
  | ReturnF  Val
  | StoreF   Val
  | FetchF   Name
  | UpdateF  Name Val
  | BlockF   (AST a)
  deriving (Generic, NFData, Eq, Show, Functor)

data AltF a = Alt CPat (AST a)
  deriving (Generic, NFData, Eq, Show, Functor)



type LPat = Val
type SimpleVal = Val
data Val
  = ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag)
  | VarTagNode    Name [SimpleVal] -- complete node (variable tag)
  | ValTag        Tag
  | Unit
  -- simple val
  | Lit Lit
  | Var Name
  -- extra
  | Loc Int
  | Undefined
  deriving (Generic, NFData, Eq, Show)

data Lit = LFloat Float
  deriving (Generic, NFData, Eq, Show)

data CPat
  = NodePat Tag [Name]
  | TagPat  Tag
  | LitPat  Lit
  deriving (Generic, NFData, Eq, Show)

data TagType = C | F | P
  deriving (Generic, NFData, Eq, Show)

data Tag = Tag TagType Name Int -- is this arity?
  deriving (Generic, NFData, Eq, Show)

