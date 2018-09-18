{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module GhcDump_Ast where

import GHC.Generics

import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Binary

import Unique (mkUnique)

-- * data definitions

type T_Text = BS8.ByteString

data Unique
  = Unique !Char !Int
  deriving (Eq, Ord, Generic)

newtype ModuleName
  = ModuleName { getModuleName :: T_Text }
  deriving (Eq, Ord, Binary, Show)

data ExternalName
  = ExternalName
    { externalModuleName  :: !ModuleName
    , externalName        :: !T_Text
    , externalUnique      :: !Unique
    , externalIdDetails   :: IdDetails
    , externalIdArity     :: !Int
    , externalIdCallArity :: !Int
    , externalIsTyVar     :: !Bool
    }
  | ForeignCall
  deriving (Eq, Ord, Generic, Show)

newtype BinderId
  = BinderId Unique
  deriving (Eq, Ord, Binary, Show)

newtype SBinder
  = SBndr { unSBndr :: Binder' SBinder BinderId }
  deriving (Eq, Ord, Generic, Show)

newtype Binder
  = Bndr { unBndr :: Binder' Binder Binder }
  deriving (Eq, Ord, Generic, Show)

data Binder' bndr var
  = Binder
    { binderName      :: !T_Text
    , binderId        :: !BinderId
    , binderIdInfo    :: IdInfo bndr var
    , binderIdDetails :: IdDetails
    , binderType      :: Type' bndr var
    }
  | TyBinder
    { binderName      :: !T_Text
    , binderId        :: !BinderId
    , binderKind      :: Type' bndr var
    }
  deriving (Eq, Ord, Generic, Show)

data IdInfo bndr var
  = IdInfo
    { idiArity         :: !Int
    , idiIsOneShot     :: Bool
    , idiUnfolding     :: Unfolding bndr var
    , idiInlinePragma  :: !T_Text
    , idiOccInfo       :: OccInfo
    , idiStrictnessSig :: !T_Text
    , idiDemandSig     :: !T_Text
    , idiCallArity     :: !Int
    }
  deriving (Eq, Ord, Generic, Show)

data Unfolding bndr var
  = NoUnfolding
  | BootUnfolding
  | OtherCon [AltCon]
  | DFunUnfolding
  | CoreUnfolding
    { unfTemplate   :: Expr' bndr var
    , unfIsValue    :: Bool
    , unfIsConLike  :: Bool
    , unfIsWorkFree :: Bool
    , unfGuidance   :: T_Text
    }
  deriving (Eq, Ord, Generic, Show)

data OccInfo
  = OccManyOccs -- | introduced in GHC 8.2
  | OccDead
  | OccOneOcc
  | OccLoopBreaker { occStrongLoopBreaker :: Bool }
  deriving (Eq, Ord, Generic, Show)

data IdDetails
  = VanillaId
  | RecSelId
  | DataConWorkId
  | DataConWrapId
  | ClassOpId
  | PrimOpId
  -- | FCallId  (these are treated as ExternalNames since they have no binding site)
  | TickBoxOpId
  | DFunId
  | CoVarId -- | introduced in GHC 8.0
  | JoinId { joinIdArity :: !Int }
  deriving (Eq, Ord, Generic, Show)

data Lit
  = MachChar      Char
  | MachStr       BS.ByteString
  | MachNullAddr
  | MachInt       Integer
  | MachInt64     Integer
  | MachWord      Integer
  | MachWord64    Integer
  | MachFloat     Rational
  | MachDouble    Rational
  | MachLabel     T_Text
  | LitInteger    Integer
  deriving (Eq, Ord, Generic, Show)

data TyCon
  = TyCon !T_Text !Unique
  deriving (Eq, Ord, Generic, Show)

type SType = Type' SBinder BinderId
type Type  = Type' Binder Binder

data Type' bndr var
  = VarTy       var
  | FunTy       (Type' bndr var) (Type' bndr var)
  | TyConApp    TyCon [Type' bndr var]
  | AppTy       (Type' bndr var) (Type' bndr var)
  | ForAllTy    bndr (Type' bndr var)
  | LitTy
  | CoercionTy
  deriving (Eq, Ord, Generic, Show)

type Module  = Module' Binder Binder
type SModule = Module' SBinder BinderId

data Module' bndr var
  = Module
    { moduleName        :: ModuleName
    , modulePhase       :: T_Text
    , moduleTopBindings :: [TopBinding' bndr var]
    }
  deriving (Generic, Show)

-- $binders
--
-- The binder story:
--
-- Things which might contain bound variables (e.g. expressions and types) have
-- a type variable which is instantiated at 'BinderId' in the serialised form or
-- 'Binder' after post-processing.
--
-- Note that bindings sites themselves are always 'Binder's.

type SExpr = Expr' SBinder BinderId
type Expr  = Expr' Binder Binder

data Expr' bndr var
  = EVar        var
  | EVarGlobal  ExternalName
  | ELit        Lit
  | EApp        (Expr' bndr var) (Expr' bndr var)
  | ETyLam      bndr (Expr' bndr var)
  | ELam        bndr (Expr' bndr var)
  | ELet        [(bndr, Expr' bndr var)] (Expr' bndr var)
  | ECase       (Expr' bndr var) bndr [Alt' bndr var]
  | EType       (Type' bndr var)
  | ECoercion
  deriving (Eq, Ord, Generic, Show)

type SAlt = Alt' SBinder BinderId
type Alt  = Alt' Binder Binder

data Alt' bndr var
  = Alt
    { altCon     :: !AltCon
    , altBinders :: [bndr]
    , altRHS     :: Expr' bndr var
    }
  deriving (Eq, Ord, Generic, Show)

data AltCon
  = AltDataCon  !(Maybe T_Text) !T_Text -- module name, data con name
  | AltLit      Lit
  | AltDefault
  deriving (Eq, Ord, Generic, Show)

type STopBinding = TopBinding' SBinder BinderId
type TopBinding  = TopBinding' Binder Binder

data TopBinding' bndr var
  = NonRecTopBinding  bndr CoreStats (Expr' bndr var)
  | RecTopBinding     [(bndr, CoreStats, Expr' bndr var)]
  deriving (Generic, Show)

data CoreStats
  = CoreStats
    { csTerms       :: !Int
    , csTypes       :: !Int
    , csCoercions   :: !Int
    , csValBinds    :: !Int
    , csJoinBinds   :: !Int
    }
  deriving (Generic, Show)

-- * utility functions

binderUniqueName :: Binder -> T_Text
binderUniqueName (Bndr b) = binderName b <> BS8.pack "_" <> BS8.pack (show u)
  where BinderId u = binderId b

moduleBindings :: Module' bndr var -> [(bndr, CoreStats, Expr' bndr var)]
moduleBindings = concatMap topBindings . moduleTopBindings

topBindings :: TopBinding' bndr var -> [(bndr, CoreStats, Expr' bndr var)]
topBindings (NonRecTopBinding a b c) = [(a,b,c)]
topBindings (RecTopBinding bs) = bs

-- * instances

instance Monoid CoreStats where
  mempty = CoreStats 0 0 0 0 0
  CoreStats a b c d e `mappend` CoreStats a' b' c' d' e' = CoreStats (a + a') (b + b') (c + c') (d + d') (e + e')

-- | This is dependent upon GHC
instance Show Unique where
 show (Unique c n) = show $ mkUnique c n

instance Binary Unique
instance Binary ExternalName
instance Binary SBinder
instance Binary Binder
instance (Binary bndr, Binary var) => Binary (Binder' bndr var)
instance (Binary bndr, Binary var) => Binary (IdInfo bndr var)
instance (Binary bndr, Binary var) => Binary (Unfolding bndr var)
instance Binary OccInfo
instance Binary IdDetails
instance Binary Lit
instance Binary TyCon
instance (Binary bndr, Binary var) => Binary (Type' bndr var)
instance (Binary bndr, Binary var) => Binary (Module' bndr var)
instance (Binary bndr, Binary var) => Binary (Expr' bndr var)
instance (Binary bndr, Binary var) => Binary (Alt' bndr var)
instance Binary AltCon
instance (Binary bndr, Binary var) => Binary (TopBinding' bndr var)
instance Binary CoreStats
