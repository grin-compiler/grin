{-# LANGUAGE LambdaCase, TemplateHaskell, RecordWildCards #-}
module Grin.Nametable
  ( Nametable
  , convert
  , restore
  ) where

import Data.Text (Text, unpack)
import Grin.Syntax
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
  ConstTagNode t vs -> ConstTagNode <$> tag t <*> mapM value vs
  VarTagNode   n vs -> VarTagNode <$> nameToIdx n <*> mapM value vs
  ValTag       t    -> ValTag <$> tag t
  Unit              -> pure Unit
  Lit l             -> Lit <$> lit l
  Var n             -> Var <$> nameToIdx n
  Undefined ty      -> pure $ Undefined ty

cpat :: CPat -> NametableM CPat
cpat = \case
  NodePat t ns -> NodePat <$> tag t <*> mapM nameToIdx ns
  LitPat  l    -> LitPat <$> lit l
  DefaultPat   -> pure DefaultPat
  TagPat  t    -> TagPat <$> tag t

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
    ProgramF es defs  -> Program <$> mapM external es <*> sequence defs
    DefF fn ps body   -> Def <$> (nameToIdx fn) <*> (mapM nameToIdx ps) <*> body
    EBindF l v r      -> EBind <$> l <*> value v <*> r
    ECaseF v alts     -> ECase <$> value v <*> sequence alts
    SAppF v ps        -> SApp <$> nameToIdx v <*> (mapM value ps)
    SReturnF v        -> SReturn <$> value v
    SStoreF v         -> SStore <$> value v
    SFetchIF n p      -> SFetchI <$> nameToIdx n <*> (pure p)
    SUpdateF n v      -> SUpdate <$> nameToIdx n <*> value v
    SBlockF body      -> SBlock <$> body
    AltF cp e         -> Alt <$> cpat cp <*> e

-- * Restore

-- | Restore names from a nametable, assuming that all the
-- identifiers are present in the table.
restore :: (Exp, Nametable) -> Exp
restore (exp, nt) = cata build exp where
  build :: ExpF Exp -> Exp
  build = \case
    ProgramF es defs  -> Program (map rexternal es) defs
    DefF fn ps body   -> Def (rname fn) (map rname ps) body
    EBindF l v r      -> EBind l (rvalue v) r
    ECaseF v alts     -> ECase (rvalue v) alts
    SAppF v ps        -> SApp (rname v) (map rvalue ps)
    SReturnF v        -> SReturn (rvalue v)
    SStoreF v         -> SStore (rvalue v)
    SFetchIF n p      -> SFetchI (rname n) p
    SUpdateF n v      -> SUpdate (rname n) (rvalue v)
    SBlockF body      -> SBlock body
    AltF cp e         -> Alt (rcpat cp) e

  rname :: Name -> Name
  rname (NI i) = maybe (error $ show i ++ " is not found") NM $ Map.lookup i nt

  rvalue :: Val -> Val
  rvalue = \case
    ConstTagNode t vs -> ConstTagNode (rtag t) (map rvalue vs)
    VarTagNode   n vs -> VarTagNode (rname n) (map rvalue vs)
    ValTag       t    -> ValTag (rtag t)
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
    TagPat  t    -> TagPat (rtag t)

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
