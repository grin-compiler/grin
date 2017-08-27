{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable #-}
module Grin where

import Data.Functor.Foldable as Foldable
import Control.DeepSeq
import Data.Map (Map)
import GHC.Generics (Generic)

type Name = String

type Prog = Map Name Def

type SimpleExp = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [Def]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       SimpleExp LPat Exp
  | ECase       Val [Alt]
  -- Simple Exp
  | SApp        Name [SimpleVal]
  | SReturn     Val
  | SStore      Val
  | SFetch      Name
--  | SFetchItem  Name Int
  | SUpdate     Name Val
  | SBlock      Exp
  -- Alt
  | Alt CPat Exp
  deriving (Generic, NFData, Eq, Show)

type LPat = Val
type SimpleVal = Val
-- TODO: use data types a la carte style to build different versions of Val?
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
  deriving (Generic, NFData, Eq, Ord, Show)

data Tag = Tag TagType Name Int -- is this arity?
  deriving (Generic, NFData, Eq, Ord, Show)

-- * shahe functors

data ExpF a
  = ProgramF  [a]
  | DefF      Name [Name] a
  -- Exp
  | EBindF    a LPat a
  | ECaseF    Val [a]
  -- Simple Expr
  | SAppF     Name [SimpleVal]
  | SReturnF  Val
  | SStoreF   Val
  | SFetchF   Name
  | SUpdateF  Name Val
  | SBlockF   a
  -- Alt
  | AltF CPat a
  deriving (Generic, NFData, Eq, Show, Functor, Foldable, Traversable)

type instance Base Exp = ExpF
instance Recursive Exp where
  project (Program  defs) = ProgramF defs
  project (Def      name args exp) = DefF name args exp
  -- Exp
  project (EBind    simpleExp lpat exp) = EBindF simpleExp lpat exp
  project (ECase    val alts) = ECaseF val alts
  -- Simple Expr
  project (SApp     name simpleVals) = SAppF name simpleVals
  project (SReturn  val) = SReturnF val
  project (SStore   val) = SStoreF val
  project (SFetch   name) = SFetchF name
  project (SUpdate  name val) = SUpdateF name val
  project (SBlock   exp) = SBlockF exp
  -- Alt
  project (Alt cpat exp) = AltF cpat exp

instance Corecursive Exp where
  embed (ProgramF  defs) = Program defs
  embed (DefF      name args exp) = Def name args exp
  -- Exp
  embed (EBindF    simpleExp lpat exp) = EBind simpleExp lpat exp
  embed (ECaseF    val alts) = ECase val alts
  -- Simple Expr
  embed (SAppF     name simpleVals) = SApp name simpleVals
  embed (SReturnF  val) = SReturn val
  embed (SStoreF   val) = SStore val
  embed (SFetchF   name) = SFetch name
  embed (SUpdateF  name val) = SUpdate name val
  embed (SBlockF   exp) = SBlock exp
  -- Alt
  embed (AltF cpat exp) = Alt cpat exp
