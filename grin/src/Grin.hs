{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE LambdaCase, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Grin where

import Data.Functor.Foldable as Foldable
import Control.DeepSeq
import Data.Map (Map)
import GHC.Generics (Generic)
import Debug.Trace (trace)
import Data.Int
import Data.Word
import Data.Text (Text)
import Data.List (isPrefixOf)
import Lens.Micro.Platform
import Data.Maybe
import qualified Data.ByteString.Short as B

data Name2
  = Name        B.ShortByteString
  | DerivedName B.ShortByteString Int
  | NewName     Name2 Int -- Block scope with shadowing support
  deriving (Ord, Eq, Show)

type Name = String

type SimpleExp = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

isPrimName :: Name -> Bool
isPrimName = isPrefixOf "_prim_"

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

pattern SFetch name = SFetchI name Nothing
pattern SFetchF name = SFetchIF name Nothing

isSimpleExp :: Exp -> Bool
isSimpleExp = \case
  SApp    _ _ -> True
  SReturn _   -> True
  SStore  _   -> True
  SFetchI _ _ -> True
  SUpdate _ _ -> True
  SBlock  _   -> True
  _           -> False

selectNodeItem :: Maybe Int -> Val -> Val
selectNodeItem Nothing val = val
selectNodeItem (Just 0) (ConstTagNode tag args) = ValTag tag
selectNodeItem (Just i) (ConstTagNode tag args) = args !! (i - 1)

type LPat = Val -- ConstTagNode, VarTagNode, ValTag, Unit, Lit, Var
type SimpleVal = Val
-- TODO: use data types a la carte style to build different versions of Val?
data Val
  = ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag) ; HIGH level GRIN
  | VarTagNode    Name [SimpleVal] -- complete node (variable tag)
  | ValTag        Tag
  | Unit                           -- HIGH level GRIN
  -- simple val
  | Lit Lit                        -- HIGH level GRIN
  | Var Name                       -- HIGH level GRIN
  -- extra
  | Loc Int   -- TODO: remove ASAP!
  | Undefined -- TODO: remove ASAP!
  deriving (Generic, NFData, Eq, Ord, Show)

isBasicValue :: Val -> Bool
isBasicValue = \case
  ValTag _ -> True
  Unit     -> True
  Lit _    -> True
  _        -> False

class FoldNames n where
  foldNames :: (Monoid m) => (Name -> m) -> n -> m

instance FoldNames Val where
  foldNames f = \case
    ConstTagNode  _tag vals -> mconcat $ foldNames f <$> vals
    VarTagNode    name vals -> mconcat $ (f name) : (foldNames f <$> vals)
    ValTag        _tag      -> mempty
    Unit                    -> mempty
    -- simple val
    Lit lit                 -> mempty
    Var name                -> f name
    -- extra
    Loc int                 -> mempty
    Undefined               -> mempty

match :: Traversal' a b -> a -> Bool
match t x = isJust $ x ^? t

isLit :: Val -> Bool
isLit = match _Lit

_Lit :: Traversal' Val Lit
_Lit f (Lit l) = Lit <$> f l
_Lit _ rest    = pure rest

_Var :: Traversal' Val Name
_Var f (Var name) = Var <$> f name
_Var _ rest       = pure rest

data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  deriving (Generic, NFData, Eq, Ord, Show)

data CPat
  = NodePat Tag [Name]  -- HIGH level GRIN
  | LitPat  Lit         -- HIGH level GRIN
  | DefaultPat          -- HIGH level GRIN
  | TagPat  Tag
  deriving (Generic, NFData, Eq, Show, Ord)

isBasicCPat :: CPat -> Bool
isBasicCPat = \case
  TagPat _ -> True
  LitPat _ -> True
  _        -> False

instance FoldNames CPat where
  foldNames f = \case
    NodePat _ names -> mconcat (map f names)
    TagPat _    -> mempty
    LitPat _    -> mempty
    DefaultPat  -> mempty

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, NFData, Eq, Ord, Show)

data Tag = Tag
  { tagType :: TagType
  , tagName :: Name
  }
  deriving (Generic, NFData, Eq, Ord, Show)

-- * shape functors

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
  | SFetchIF  Name (Maybe Int)
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
  project (SFetchI  name index) = SFetchIF name index
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
  embed (SFetchIF  name index) = SFetchI name index
  embed (SUpdateF  name val) = SUpdate name val
  embed (SBlockF   exp) = SBlock exp
  -- Alt
  embed (AltF cpat exp) = Alt cpat exp

type instance Base Val = ValF

data ValF a
  = ConstTagNodeF  Tag  [a] -- complete node (constant tag)
  | VarTagNodeF    Name [a] -- complete node (variable tag)
  | ValTagF        Tag
  | UnitF
  -- simple val
  | LitF Lit
  | VarF Name
  -- extra
  | LocF Int
  | UndefinedF
  deriving (Generic, NFData, Eq, Show, Functor, Foldable, Traversable)

instance Recursive Val where
  project = \case
    ConstTagNode  tag  simpleVals -> ConstTagNodeF tag simpleVals
    VarTagNode    name simpleVals -> VarTagNodeF name simpleVals
    ValTag        tag             -> ValTagF tag
    Unit                          -> UnitF

    Lit lit    -> LitF lit
    Var name   -> VarF name

    Loc int    -> LocF int
    Undefined  -> UndefinedF

instance Corecursive Val where
  embed = \case
    ConstTagNodeF  tag  as -> ConstTagNode tag  as
    VarTagNodeF    name as -> VarTagNode   name as
    ValTagF        tag     -> ValTag       tag
    UnitF                  -> Unit

    LitF lit   -> Lit lit
    VarF name  -> Var name

    LocF int   -> Loc int
    UndefinedF -> Undefined

data NamesInExpF e a = NamesInExpF
  { namesExp   :: ExpF e
  , namesNameF :: Name -> a
  } deriving (Functor)

instance Foldable (NamesInExpF Exp) where
  foldr f b (NamesInExpF expf fn) = case expf of
    ProgramF  _            -> b
    DefF      name names _ -> foldr f b $ map fn (name:names)
    -- Exp
    EBindF    se lPat _  -> foldr f (foldr f b (NamesInExpF (project se) fn)) $ namesInVal lPat
    ECaseF    val _      -> foldr f b $ namesInVal val
    -- Simple Expr

    -- Does not collect function names in application
    SAppF     _name simpleVals -> foldr f b $ (map fn $ concatMap (foldNames list) simpleVals)
    SReturnF  val -> foldr f b $ namesInVal val
    SStoreF   val -> foldr f b $ namesInVal val
    SFetchIF  name mpos -> f (fn name) b
    SUpdateF  name val  -> foldr f b (fn name : namesInVal val)
    SBlockF   _ -> b
    -- Alt
    AltF cPat _ -> foldr f b $ map fn $ foldNames list cPat
    where
      namesInVal = map fn . foldNames list
      list x = [x]

dCoAlg :: (a -> String) -> (a -> ExpF b) -> (a -> ExpF b)
dCoAlg dbg f = f . (\x -> trace (dbg x) x)

dAlg :: (b -> String) -> (ExpF a -> b) -> (ExpF a -> b)
dAlg dbg f = (\x -> trace (dbg x) x) . f

isConstant :: Val -> Bool
isConstant = cata $ \case
  ConstTagNodeF  tag params -> and params
  ValTagF        tag        -> True
  UnitF                     -> True
  LitF lit                  -> True
  _                         -> False

hasConstant :: Val -> Bool
hasConstant = cata $ \case
  ValTagF{} -> True
  UnitF     -> True
  LitF{}    -> True
  v         -> or v

isAllVar :: Val -> Bool
isAllVar = cata $ \case
  ConstTagNodeF _ params  -> and params
  VarF{}                  -> True
  _                       -> False
