{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Grin.Syntax
  ( module Grin.Syntax
  , module Grin.SyntaxDefs
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



-- * GRIN Externals, i.e. primops and foreign functions

data Ty
  = TyCon     Name [Ty]
  | TyVar     Name
  | TySimple  SimpleType
  deriving (Generic, Data, NFData, Eq, Ord, Show)

data ExternalKind
  = PrimOp -- ^ Implemented in the internal code generator
  | FFI    -- ^ Implemented in C and linked during the linker phase
  deriving (Generic, Data, NFData, Eq, Ord, Show)

data External
  = External
  { eName       :: Name
  , eRetType    :: Ty
  , eArgsType   :: [Ty]
  , eEffectful  :: Bool
  , eKind       :: ExternalKind
  }
  deriving (Generic, Data, NFData, Eq, Ord, Show)

isExternalName :: [External] -> Name -> Bool
isExternalName es n = n `Prelude.elem` (eName <$> es)

-- * GRIN Literal

-- QUESTION: Now #undefined can be pattern matched on.
-- Should the linter warn about this?
data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  | LString Text
  | LChar   Char
  deriving (Generic, Data, NFData, Eq, Ord, Show)

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
  | Undefined     Type
  deriving (Generic, Data, NFData, Eq, Ord, Show)

-- See: https://github.com/ekmett/recursion-schemes/blob/master/Data/Functor/Foldable/TH.hs#L31
makeBaseFunctor ''Val

-- * Case Pattern

data CPat
  = NodePat Tag [Name]  -- HIGH level GRIN
  | LitPat  Lit         -- HIGH level GRIN
  | DefaultPat          -- HIGH level GRIN
  | TagPat  Tag
  deriving (Generic, Data, NFData, Eq, Show, Ord)

-- * GRIN Expression

type SimpleExp = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [External] [Def]
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
  deriving (Generic, Data, NFData, Eq, Ord, Show)

externals :: Exp -> [External]
externals = \case
  Program es _ -> es
  _            -> []

-- * Binary instances

deriving instance Binary Name
deriving instance Binary ExternalKind
deriving instance Binary External
deriving instance Binary Ty
deriving instance Binary SimpleType
deriving instance Binary TagType
deriving instance Binary Type
deriving instance Binary Lit
deriving instance Binary Tag
deriving instance Binary CPat
deriving instance Binary Val
deriving instance Binary Exp

instance Binary a => Binary (Vector a) where
  get = Data.Vector.fromList <$> get
  put = put . Data.Vector.toList

-- See: https://github.com/ekmett/recursion-schemes/blob/master/Data/Functor/Foldable/TH.hs#L31
makeBaseFunctor ''Exp

deriving instance Show a  => Show (ExpF a)
deriving instance Eq a    => Eq   (ExpF a)
deriving instance Ord a   => Ord  (ExpF a)

pattern SFetch name = SFetchI name Nothing
pattern SFetchF name = SFetchIF name Nothing

pattern BoolPat b = LitPat (LBool b)

_AltCPat :: Traversal' Exp CPat
_AltCPat f (Alt p e) = (`Alt` e) <$> f p
_AltCPat _ other     = pure other

_AltFCPat :: Traversal' (ExpF a) CPat
_AltFCPat f (AltF p e) = (`AltF` e) <$> f p
_AltFCPat _ other      = pure other

_CPatNodeTag :: Traversal' CPat Tag
_CPatNodeTag f (NodePat tag args) = (`NodePat` args) <$> f tag
_CPatNodeTag _ other              = pure other

_CPatLit :: Traversal' CPat Lit
_CPatLit f (LitPat lit) = LitPat <$> f lit
_CPatLit _ other        = pure other

_CPatDefault :: Traversal' CPat ()
_CPatDefault f DefaultPat = const DefaultPat <$> f ()
_CPatDefault _ other      = pure other

_ValVar :: Traversal' Val Name
_ValVar f (Var name) = Var <$> f name
_ValVar _ other      = pure other

_TyCon :: Traversal' Ty (Name, [Ty])
_TyCon f (TyCon n ts) = uncurry TyCon <$> f (n, ts)
_TyCon _ other        = pure other

_TyVar :: Traversal' Ty Name
_TyVar f (TyVar n) = TyVar <$> f n
_TyVar _ other     = pure other

_TySimple :: Traversal' Ty SimpleType
_TySimple f (TySimple t) = TySimple <$> f t
_TySimple _ other        = pure other
