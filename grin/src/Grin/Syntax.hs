{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, OverloadedStrings #-}
module Grin.Syntax where

import Data.Functor.Foldable.TH
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Int
import Data.Word
import qualified Data.ByteString.Short as B
import Lens.Micro.Platform
import Data.Text.Short (ShortText, isPrefixOf)

data Name2
  = Name        B.ShortByteString
  | DerivedName B.ShortByteString Int
  | NewName     Name2 Int -- Block scope with shadowing support
  deriving (Ord, Eq, Show)

type Name = ShortText

isPrimName :: Name -> Bool
isPrimName = isPrefixOf "_prim_"

-- * GRIN Tag

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, NFData, Eq, Ord, Show)

data Tag = Tag
  { tagType :: TagType
  , tagName :: Name
  }
  deriving (Generic, NFData, Eq, Ord, Show)

-- * GRIN Literal

data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  | LString String
  deriving (Generic, NFData, Eq, Ord, Show)

-- * GRIN Value

type LPat = Val -- ConstTagNode, VarTagNode, ValTag, Unit, Lit, Var
type SimpleVal = Val

data Val
  = ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag) ; HIGH level GRIN
  | VarTagNode    Name [SimpleVal] -- complete node (variable tag)
  | ValTag        Tag
  | Unit                           -- HIGH level GRIN
  -- simple val
  | Lit Lit                        -- HIGH level GRIN
  | Var Name                       -- HIGH level GRIN
  deriving (Generic, NFData, Eq, Ord, Show)

-- See: https://github.com/ekmett/recursion-schemes/blob/master/Data/Functor/Foldable/TH.hs#L31
makeBaseFunctor ''Val

-- * Case Pattern

data CPat
  = NodePat Tag [Name]  -- HIGH level GRIN
  | LitPat  Lit         -- HIGH level GRIN
  | DefaultPat          -- HIGH level GRIN
  | TagPat  Tag
  deriving (Generic, NFData, Eq, Show, Ord)

-- * GRIN Expression

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
  | SFetchI     Name (Maybe Int) -- fetch a full node or a single node item in low level GRIN
  | SUpdate     Name Val
  | SBlock      Exp
  -- Alt
  | Alt CPat Exp
  deriving (Generic, NFData, Eq, Ord, Show)

-- See: https://github.com/ekmett/recursion-schemes/blob/master/Data/Functor/Foldable/TH.hs#L31
makeBaseFunctor ''Exp

deriving instance Show a  => Show (ExpF a)
deriving instance Eq a    => Eq   (ExpF a)
deriving instance Ord a   => Ord  (ExpF a)

pattern SFetch name = SFetchI name Nothing
pattern SFetchF name = SFetchIF name Nothing

_AltCPat :: Traversal' Exp CPat
_AltCPat f (Alt p e) = (`Alt` e) <$> f p
_AltCPat _ other     = pure other

_CPatNodeTag :: Traversal' CPat Tag
_CPatNodeTag f (NodePat tag args) = (`NodePat` args) <$> f tag
_CPatNodeTag _ other              = pure other

_CPatLit :: Traversal' CPat Lit
_CPatLit f (LitPat lit) = LitPat <$> f lit
_CPatLit _ other        = pure other

_CPatDefault :: Traversal' CPat ()
_CPatDefault f DefaultPat = const DefaultPat <$> f ()
_CPatDefault _ other      = pure other
