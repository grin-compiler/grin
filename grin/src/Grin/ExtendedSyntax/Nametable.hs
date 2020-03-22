{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards #-}
module Grin.ExtendedSyntax.Nametable
  ( Nametable
  , convert
  , restore
  ) where

import Data.Text (Text, unpack)
import Grin.ExtendedSyntax.Syntax
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable
import Control.Monad.State
import Lens.Micro.Platform
import Data.Bifunctor

-- * Convert

type Nametable = Map.Map Int Text
type ReverseTable = Map.Map Text Int

data NS = NS
  { _latest       :: !Int
  , _nametable    :: Nametable
  , _reverseTable :: ReverseTable
  }

emptyNS :: NS
emptyNS = NS 0 Map.empty Map.empty

makeLenses ''NS

type NametableM = State NS

nameToIdx :: Name -> NametableM Name
nameToIdx ni@NI{} = pure ni
nameToIdx (NM n) = do
  mIdx <- use (reverseTable . at n)
  case mIdx of
    Just i  -> pure $ NI i
    Nothing -> do
      idx <- latest <<%= succ -- Modifies the state and returns the old value
      nametable       %= Map.insert idx n
      reverseTable    %= Map.insert n idx
      pure $ NI idx

tag :: Tag -> NametableM Tag
tag (Tag tt tn) = Tag tt <$> nameToIdx tn

lit :: Lit -> NametableM Lit
lit = pure -- TODO: Handle string literals

value :: Val -> NametableM Val
value = \case
  ConstTagNode t vs -> ConstTagNode <$> tag t <*> mapM nameToIdx vs
  Unit              -> pure Unit
  Lit l             -> Lit <$> lit l
  Var n             -> Var <$> nameToIdx n
  Undefined ty      -> pure $ Undefined ty

cpat :: CPat -> NametableM CPat
cpat = \case
  NodePat t ns -> NodePat <$> tag t <*> mapM nameToIdx ns
  LitPat  l    -> LitPat <$> lit l
  DefaultPat   -> pure DefaultPat

ty :: Ty -> NametableM Ty
ty = \case
  TyCon    n ts -> TyCon <$> nameToIdx n <*> mapM ty ts
  TyVar    n    -> TyVar <$> nameToIdx n
  TySimple st   -> pure $ TySimple st

external :: External -> NametableM External
external (External{..}) =
  External <$> nameToIdx eName
           <*> ty eRetType
           <*> mapM ty eArgsType
           <*> (pure eEffectful)
           <*> (pure eKind)

-- | Convert Names in the expression to Int identifiers and create
-- an associated name table.
convert :: Exp -> (Exp, Nametable)
convert = second (view nametable) . flip runState emptyNS . cata build where
  build :: ExpF (NametableM Exp) -> NametableM Exp
  build = \case
    ProgramF es defs            -> Program <$> mapM external es <*> sequence defs
    DefF fn ps body             -> Def <$> (nameToIdx fn) <*> (mapM nameToIdx ps) <*> body
    EBindF l (VarPat v) r       -> EBind <$> l <*> (VarPat <$> nameToIdx v) <*> r
    EBindF l (AsPat t vars v) r -> EBind <$> l <*> (AsPat <$> tag t <*> mapM nameToIdx vars <*> nameToIdx v) <*> r
    ECaseF v alts               -> ECase <$> nameToIdx v <*> sequence alts
    SAppF v ps                  -> SApp <$> nameToIdx v <*> (mapM nameToIdx ps)
    SReturnF v                  -> SReturn <$> value v
    SStoreF v                   -> SStore <$> nameToIdx v
    SFetchF ptr                 -> SFetch <$> nameToIdx ptr
    SUpdateF ptr var            -> SUpdate <$> nameToIdx ptr <*> nameToIdx var
    SBlockF body                -> SBlock <$> body
    AltF cp n e                 -> Alt <$> cpat cp <*> nameToIdx n <*> e

-- * Restore

-- | Restore names from a nametable, assuming that all the
-- identifiers are present in the table.
restore :: (Exp, Nametable) -> Exp
restore (exp, nt) = cata build exp where
  build :: ExpF Exp -> Exp
  build = \case
    ProgramF es defs           -> Program (map rexternal es) defs
    DefF fn ps body            -> Def (rname fn) (map rname ps) body
    EBindF l (VarPat v) r      -> EBind l (VarPat $ rname v) r
    EBindF l (AsPat t vs v) r  -> EBind l (AsPat (rtag t) (map rname vs) (rname v)) r
    ECaseF v alts              -> ECase (rname v) alts
    SAppF v ps                 -> SApp (rname v) (map rname ps)
    SReturnF v                 -> SReturn (rvalue v)
    SStoreF v                  -> SStore (rname v)
    SFetchF ptr                -> SFetch (rname ptr)
    SUpdateF ptr var           -> SUpdate (rname ptr) (rname var)
    SBlockF body               -> SBlock body
    AltF cp n e                -> Alt (rcpat cp) (rname n) e

  rname :: Name -> Name
  rname (NI i) = maybe (error $ show i ++ " is not found") NM $ Map.lookup i nt

  rvalue :: Val -> Val
  rvalue = \case
    ConstTagNode t vs -> ConstTagNode (rtag t) (map rname vs)
    Unit              -> Unit
    Lit l             -> Lit (rlit l)
    Var n             -> Var (rname n)
    Undefined ty      -> Undefined ty

  rlit :: Lit -> Lit
  rlit = id -- TODO: Handle String literals

  rtag :: Tag -> Tag
  rtag (Tag tt tn) = Tag tt (rname tn)

  rcpat :: CPat -> CPat
  rcpat = \case
    NodePat t ns -> NodePat (rtag t) (map rname ns)
    LitPat  l    -> LitPat (rlit l)
    DefaultPat   -> DefaultPat

  rexternal :: External -> External
  rexternal External{..} =
    External (rname eName)
             (rty eRetType)
             (map rty eArgsType)
             eEffectful
             eKind

  rty :: Ty -> Ty
  rty = \case
    TyCon    n ts -> TyCon (rname n) (map rty ts)
    TyVar    n    -> TyVar (rname n)
    TySimple st   -> TySimple st
