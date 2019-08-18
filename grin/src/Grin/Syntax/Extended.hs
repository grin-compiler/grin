{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Grin.Syntax.Extended
  ( module Grin.Syntax.Extended
  , module Grin.SyntaxDefs

  -- reexports from Grin.Syntax
  , Ty(..)
  , ExternalKind(..)
  , External(..)
  , Lit(..)
  , CPat(..)
  , _CPatNodeTag
  , _TyCon
  , _TyVar
  , _TySimple
  , _CPatLit
  , _CPatDefault
  , isExternalName
  ) where

import Data.Data
import Control.DeepSeq
import Data.Binary
import Data.Functor.Foldable.TH
import Data.Int
import Data.Text (Text, isPrefixOf)
import Data.Vector
import Data.Word
import GHC.Generics (Generic)
import Lens.Micro.Platform
import qualified Data.ByteString.Short as B

import Grin.SyntaxDefs
import Grin.TypeEnvDefs

import Grin.Syntax
  ( Ty(..)
  , ExternalKind(..)
  , External(..)
  , Lit(..)
  , CPat(..)
  , _CPatNodeTag
  , _TyCon
  , _TyVar
  , _TySimple
  , _CPatLit
  , _CPatDefault
  , isExternalName
  )

-- * GRIN Value

data Val
  -- CHANGE: Name
  = ConstTagNode  Tag  [Name]
  -- CHANGE: Name
  | Unit
  -- simple val
  | Lit Lit
  | Var Name
  | Undefined     Type
  deriving (Generic, Data, NFData, Eq, Ord, Show)

-- See: https://github.com/ekmett/recursion-schemes/blob/master/Data/Functor/Foldable/TH.hs#L31
makeBaseFunctor ''Val

-- * Binding pattern

data BPat
  = VarPat { _bPatVar :: Name }
  | AsPat  { _bPatVar :: Name
           , _bPatVal :: Val
           }
  deriving (Generic, Data, NFData, Eq, Show, Ord)

makeLenses ''BPat

-- * GRIN Expression

type SimpleExp = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [External] [Def]
  | Def         Name [Name] Exp
  -- Exp
  -- CHANGE: BPat
  | EBind       SimpleExp BPat Exp
  -- CHANGE: Name
  | ECase       Name [Alt]
  -- Simple Exp
  -- CHANGE: Name
  | SApp        Name [Name]
  | SReturn     Val
  -- CHANGE: Name
  | SStore      Name
  | SFetch      Name
  -- CHANGE: Name
  | SUpdate     Name Name
  | SBlock      Exp
  -- Alt
  | Alt CPat Exp
  deriving (Generic, Data, NFData, Eq, Ord, Show)

externals :: Exp -> [External]
externals = \case
  Program es _ -> es
  _            -> []

-- * Binary instances

deriving instance Binary BPat
deriving instance Binary Val
deriving instance Binary Exp

-- See: https://github.com/ekmett/recursion-schemes/blob/master/Data/Functor/Foldable/TH.hs#L31
makeBaseFunctor ''Exp

deriving instance Show a  => Show (ExpF a)
deriving instance Eq a    => Eq   (ExpF a)
deriving instance Ord a   => Ord  (ExpF a)


pattern BoolPat b = LitPat (LBool b)

_AltCPat :: Traversal' Exp CPat
_AltCPat f (Alt p e) = (`Alt` e) <$> f p
_AltCPat _ other     = pure other

_AltFCPat :: Traversal' (ExpF a) CPat
_AltFCPat f (AltF p e) = (`AltF` e) <$> f p
_AltFCPat _ other      = pure other

_ValVar :: Traversal' Val Name
_ValVar f (Var name) = Var <$> f name
_ValVar _ other      = pure other

_OnlyVarPat :: Traversal' BPat Name
_OnlyVarPat f (VarPat v) = VarPat <$> f v
_OnlyVarPat _ other      = pure other

_BPatVar :: Traversal' BPat Name
_BPatVar f (AsPat v val) = AsPat <$> f v <*> pure val
_BPatVar f (VarPat v)    = VarPat <$> f v
