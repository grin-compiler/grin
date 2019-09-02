{-# LANGUAGE StandaloneDeriving, DeriveGeneric, LambdaCase #-}
module Test.ExtendedSyntax.New.Grammar where

import qualified Grin.ExtendedSyntax.Grin as Grin
import Test.QuickCheck (NonEmptyList(..))
import GHC.Generics

-- NOTE: Generates stuff convertible to the NEW AST

data Name = Name { unName :: Grin.Name }
  deriving (Eq, Generic, Show)

deriving instance Generic (NonEmptyList a)

data Prog = Prog (NonEmptyList Def)
  deriving (Generic, Show)

data Def = Def Name [Name] Exp
  deriving (Generic, Show)

data Exp
  = EBind SExp BPat Exp
  | ECase Name (NonEmptyList Alt)
  | SExp SExp
  deriving (Generic, Show)

data Alt = Alt Grin.CPat Exp
  deriving (Generic, Show)

data SExp
  = SApp     Name [Name]
  | SReturn  Val
  | SStore   Name
  | SFetch   Name
  | SUpdate  Name Name
  | SBlock   Exp
  deriving (Generic, Show)

data Val
  = ConstTagNode  Grin.Tag [Name]
  | Unit
  | SimpleVal SimpleVal
  deriving (Eq, Generic, Show)

data SimpleVal
  = Lit Grin.Lit
  | Var Name
  deriving (Eq, Generic, Show)

data BPat
  = VarPat Name
  | AsPat  Name Val
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
    ConstTagNode  tag  args -> Grin.ConstTagNode tag (map toName args)
    Unit                    -> Grin.Unit
    SimpleVal     simpleVal -> asVal simpleVal

instance AsVal SimpleVal where
  asVal = \case
    Lit lit  -> Grin.Lit lit
    Var name -> Grin.Var (toName name)

class AsBPat t where
  asBPat :: t -> Grin.BPat

instance AsBPat BPat where
  asBPat (VarPat v)    = Grin.VarPat (toName v)
  asBPat (AsPat v val) = Grin.AsPat (toName v) (asVal val)

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
    SApp     name args -> Grin.SApp (toName name) (map toName args)
    SReturn  val -> Grin.SReturn (asVal val)
    SStore   var -> Grin.SStore (toName var)
    SFetch   ptr -> Grin.SFetch (toName ptr)
    SUpdate  ptr var -> Grin.SUpdate (toName ptr) (toName var)
    SBlock   exp -> Grin.SBlock (asExp exp)

instance AsExp Exp where
  asExp = \case
    EBind sexp bpat exp -> Grin.EBind (asExp sexp) (asBPat bpat) (asExp exp)
    ECase var alts      -> Grin.ECase (toName var) (asExp <$> getNonEmpty alts)
    SExp sexp           -> asExp sexp

instance AsExp Alt where
  asExp = \case
    Alt cpat exp -> Grin.Alt cpat (asExp exp)
