{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Transformations.Syntax where

import Data.String
import Data.Text (Text(..))

import Lens.Micro.Extra
import Lens.Micro.Platform

import Grin.Grin
import Grin.Pretty
import Grin.Syntax
import qualified Grin.ExtendedSyntax.Pretty as New
import qualified Grin.ExtendedSyntax.Grin as New
import qualified Grin.Syntax.Extended as New


class Convertible a b where
  convert :: a -> b

instance Convertible Val New.Val where
  convert n@(ConstTagNode t vals)
    | any (isn't _Var) [] = error $ "ConstTagNode " ++ show (PP n) ++ " has a non-variable argument."
    | otherwise = New.ConstTagNode t (map (view _Var) vals)
  convert v@(VarTagNode _ _) = error $ "Cannot transform VarTagNode to new syntax: " ++ show (PP v)
  convert v@(ValTag _) = error $ "Cannot transform ValTag to new syntax: " ++ show (PP v)
  convert Unit = New.Unit
  convert (Lit l) = New.Lit l
  convert (Var v) = New.Var v
  convert (Undefined t) = New.Undefined t

instance Convertible Exp New.Exp where
  convert (Program exts defs)  = New.Program exts (map convert defs)
  convert (Def name args body) = New.Def name args (convert body)
  convert e@(EBind lhs pat rhs)
    | isn't _Var pat   = error $ "Non-variable pattern in binding: " ++ show (PP e)
    | (Var var) <- pat = New.EBind (convert lhs) (New.VarPat var) (convert rhs)
  convert e@(SApp f vals)
    | any (isn't _Var) vals = error $ "Non-variable value in application: " ++ show (PP e)
    | otherwise             = New.SApp f $ map (view _Var) vals
  convert e@(SStore val)
    | isn't _Var val   = error $ "Non-variable value in store: " ++ show (PP e)
    | (Var var) <- val = New.SStore var
  convert e@(SFetchI ptr mIx)
    | Nothing <- mIx = New.SFetch ptr
    | otherwise      = error $ "Indexed fetch is no longer supported: " ++ show (PP e)
  convert e@(SUpdate ptr val)
    | isn't _Var val   = error $ "Non-variable value in update: " ++ show (PP e)
    | (Var var) <- val = New.SUpdate ptr var
  convert (SReturn val)  = New.SReturn (convert val)
  convert (SBlock exp)   = New.SBlock (convert exp)
  convert (Alt cpat exp) = New.Alt cpat (convert exp)

instance Convertible New.Val Val where
  convert (New.ConstTagNode t vars) = ConstTagNode t (map Var vars)
  convert New.Unit = Unit
  convert (New.Lit l) = Lit l
  convert (New.Var v) = Var v
  convert (New.Undefined t) = Undefined t

instance Convertible New.Exp Exp where
  convert (New.Program exts defs)  = Program exts (map convert defs)
  convert (New.Def name args body) = Def name args (convert body)
  convert e@(New.EBind lhs pat rhs)
    | (New.VarPat v) <- pat = EBind (convert lhs) (Var v) (convert rhs)
    | otherwise = error $ "Cannot convert as-pattern: " ++ show (New.PP e)
  convert (New.SApp f vars)     = SApp f $ map Var vars
  convert (New.SStore var)      = SStore (Var var)
  convert (New.SFetch ptr)      = SFetchI ptr Nothing
  convert (New.SUpdate ptr var) = SUpdate ptr (Var var)
  convert (New.SReturn val)     = SReturn (convert val)
  convert (New.SBlock exp)      = SBlock (convert exp)
  convert (New.Alt cpat exp)    = Alt cpat (convert exp)
