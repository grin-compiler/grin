{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Frontend.Lambda where

import Data.Int
import Data.Word
import Data.Functor.Foldable as Foldable
import Data.Functor.Foldable.TH

type Name = String

type Atom = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [Def]
  -- Binding
  | Def         Name [Name] Exp
  -- Exp
  | App         Name [Atom]
  | Case        Atom [Alt]
  | Let         [(Name, Exp)] Exp -- lazy let
  | LetS        Name Exp Exp      -- strict let
  | Con         Tag [Atom]
  -- Atom
  | Var         Name
  | Lit         Lit
  -- Alt
  | Alt         Pat Exp
  deriving (Eq, Ord, Show)

data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  deriving (Eq, Ord, Show)

data Pat
  = NodePat Tag [Name]
  | LitPat  Lit
  | DefaultPat
  deriving (Eq, Show, Ord)

type Tag = Name

-- TODO: do we need lambda?

makeBaseFunctor ''Exp
