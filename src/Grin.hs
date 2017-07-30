{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Grin where

import Control.DeepSeq
import Data.Map (Map)
import GHC.Generics (Generic)

type Name = String

type Prog = Map Name Def

data Def = Def Name [Name] Exp
  deriving Show

data Exp
  = Bind  SimpleExp LPat Exp
  | Case  Val [Alt]
  | SExp  SimpleExp
  deriving (Generic, NFData, Eq, Show)

data SimpleExp
  = App     Name [SimpleVal]
  | Return  Val
  | Store   Val
  | Fetch   Name
--  | FetchI  Name Int -- fetch node component
  | Update  Name Val
  | Block   Exp
  deriving (Generic, NFData, Eq, Show)

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

data Alt = Alt CPat Exp
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

