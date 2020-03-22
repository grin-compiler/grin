{-# LANGUAGE StandaloneDeriving, DeriveGeneric, LambdaCase #-}
module Test.ExtendedSyntax.Old.Grammar where

import qualified Grin.Grin as Grin
import Test.QuickCheck (NonEmptyList(..))
import GHC.Generics

-- NOTE: Still generates stuff convertible to OLD AST!!

data Name = Name { unName :: Grin.Name }
  deriving (Eq, Generic, Show)

deriving instance Generic (NonEmptyList a)

data Prog = Prog (NonEmptyList Def)
  deriving (Generic, Show)

data Def = Def Name [Name] Exp
  deriving (Generic, Show)

data Exp
  = EBind SExp LPat Exp
  | ECase Val (NonEmptyList Alt)
  | SExp SExp
  deriving (Generic, Show)

data Alt = Alt Grin.CPat Exp
  deriving (Generic, Show)

data SExp
  = SApp     Name [SimpleVal]
  | SReturn  Val
  | SStore   Val
  | SFetch   Name
  | SUpdate  Name Val
  | SBlock   Exp
  deriving (Generic, Show)

data Val
  = ConstTagNode  Grin.Tag [SimpleVal]
  | Unit
  | SimpleVal SimpleVal
  deriving (Eq, Generic, Show)

data SimpleVal
  = Lit Grin.Lit
  | Var Name
  deriving (Eq, Generic, Show)

data LPat
  = LPatVal  Val
  | LPatSVal SimpleVal
  deriving (Generic, Show)

type Loc = Int

data ExtraVal
  = Loc Loc
  deriving (Eq, Generic, Show)


toName (Name n) = n

class AsVal t where
  asVal :: t -> Grin.Val

instance AsVal Val where
  asVal = \case
    ConstTagNode  tag  simpleVals -> Grin.ConstTagNode tag (asVal <$> simpleVals)
    Unit                          -> Grin.Unit
    SimpleVal     simpleVal       -> asVal simpleVal

instance AsVal SimpleVal where
  asVal = \case
    Lit lit  -> Grin.Lit lit
    Var name -> Grin.Var (toName name)

instance AsVal LPat where
  asVal = \case
    LPatVal  val  -> asVal val
    LPatSVal sval -> asVal sval


class AsExp t where
  asExp :: t -> Grin.Exp

instance AsExp Prog where
  asExp = \case
    Prog defs -> Grin.Program [] (asExp <$> getNonEmpty defs)

instance AsExp Def where
  asExp = \case
    Def name params exp -> Grin.Def (toName name) (toName <$> params) (asExp exp)

instance AsExp SExp where
  asExp = \case
    SApp     name simpleVals -> Grin.SApp (toName name) (asVal <$> simpleVals)
    SReturn  val -> Grin.SReturn (asVal val)
    SStore   val -> Grin.SStore (asVal val)
    SFetch   name -> Grin.SFetchI (toName name) Nothing
    SUpdate  name val -> Grin.SUpdate (toName name) (asVal val)
    SBlock   exp -> Grin.SBlock (asExp exp)

instance AsExp Exp where
  asExp = \case
    EBind sexp lpat exp -> Grin.EBind (asExp sexp) (asVal lpat) (asExp exp)
    ECase val alts      -> Grin.ECase (asVal val) (asExp <$> getNonEmpty alts)
    SExp sexp           -> asExp sexp

instance AsExp Alt where
  asExp = \case
    Alt cpat exp -> Grin.Alt cpat (asExp exp)
