module Grin where

import Data.Map (Map)

type Name = String

type Prog = Map Name Def

data Def = Def Name [Name] Exp
  deriving Show

data Exp
  = Bind  SimpleExp LPat Exp
  | Case  Val [Alt]
  | SExp  SimpleExp
  deriving Show

data SimpleExp
  = App     Name [SimpleVal]
  | Return  Val
  | Store   Val
  | Fetch   Name
--  | FetchI  Name Int -- fetch node component
  | Update  Name Val
  | Block   Exp
  deriving Show

type LPat = Val
type SimpleVal = Val
data Val
  = TagNode   Tag  [SimpleVal]
  | VarNode   Name [SimpleVal]
  | ValTag    Tag
  | Unit
  -- simple val
  | Lit Lit
  | Var Name
  -- extra
  | Loc Int
  | Undefined
  deriving (Eq,Show)

data Lit = LFloat Float
  deriving (Eq,Show)

data Alt = Alt CPat Exp
  deriving Show

data CPat
  = NodePat Tag [Name]
  | TagPat  Tag
  | LitPat  Lit
  deriving Show

data TagType = C | F | P
  deriving (Eq,Show)

data Tag = Tag TagType Name Int
  deriving (Eq,Show)

