{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Grin2 where

import Control.DeepSeq
import Data.Map (Map)
import GHC.Generics (Generic)

type Name = String

type Prog = Map Name Def

data Def = Def Name [Name] Exp

type SimpleExp = Exp
type Alt = Exp
data Exp
  = Bind  SimpleExp LPat Exp
  | Case  Val [Alt]
  -- SimpleExp
  | App     Name [SimpleVal]
  | Return  Val
  | Store   Val
  | Fetch   Name
  | Update  Name Val
  | Block   Exp
  -- Alt
  | Alt CPat Exp

data ExpKind = E | S | A
data ExpG (a :: ExpKind) where
  BindG :: ExpG S -> LPat -> ExpG E -> ExpG E    
  CaseG :: Val -> [ExpG A]          -> ExpG E
  SExpG :: ExpG S                   -> ExpG E

  AppG    :: Name -> [SimpleVal]    -> ExpG S
  ReturnG :: Val                    -> ExpG S
  StoreG  :: Val                    -> ExpG S
  FetchG  :: Name                   -> ExpG S
  UpdateG :: Name -> Val            -> ExpG S
  BlockG  :: ExpG E                 -> ExpG S

  AltG    :: CPat -> ExpG E         -> ExpG A

data ExpF (a :: ExpKind) (b :: ExpKind -> *) where
  BindF :: b S -> LPat -> b E    -> ExpF E b
  CaseF :: Val -> [b A]          -> ExpF E b
  SExpF :: b S                   -> ExpF E b 

  AppF    :: Name -> [SimpleVal] -> ExpF S b
  ReturnF :: Val                 -> ExpF S b
  StoreF  :: Val                 -> ExpF S b
  FetchF  :: Name                -> ExpF S b
  UpdateF :: Name -> Val         -> ExpF S b
  BlockF  :: b E                 -> ExpF S b

  AltF    :: CPat -> b E         -> ExpF A b

data K b a = K b
instance Functor (K a) where
  fmap f (K x) = K x

type LPat = Val
type SimpleVal = Val
data Val
  = ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag)
  | VarTagNode    Name [SimpleVal] -- complete node (variable tag)
  | ValTag        Tag
  | Unit
  -- simple val
  | Lit Lit
  | Var Name
  -- extra
  | Loc Int
  | Undefined

data Lit = LFloat Float

data CPat
  = NodePat Tag [Name]
  | TagPat  Tag
  | LitPat  Lit

data TagType = C | F | P

data Tag = Tag TagType Name Int -- is this arity?
