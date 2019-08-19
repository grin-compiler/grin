{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Transformations.Syntax where

import Data.String
import Data.Text (Text(..))
import Data.Functor.Foldable as Foldable

import Control.Monad

import Lens.Micro.Extra
import Lens.Micro.Platform

import Grin.Grin
import Grin.Pretty
import Grin.Syntax
import qualified Grin.ExtendedSyntax.Pretty as New
import qualified Grin.ExtendedSyntax.Grin as New
import qualified Grin.Syntax.Extended as New

import Transformations.Names
import Transformations.BindNormalisation
import Transformations.Simplifying.ProducerNameIntroduction
import Transformations.Simplifying.BindingPatternSimplification

-- TODO: remove these
import Test.QuickCheck
import Test.ExtendedSyntax.Test()
import qualified Test.ExtendedSyntax.Grammar as G

class Convertible a b where
  convert :: a -> b

instance Convertible Val New.Val where
  convert n@(ConstTagNode t vals)
    | any (isn't _Var) [] = error $ "ConstTagNode " ++ show (PP n) ++ " has a non-variable argument."
    | otherwise           = New.ConstTagNode t (map (view _Var) vals)
  convert v@(VarTagNode _ _) = error $ "Cannot transform VarTagNode to new syntax: " ++ show (PP v)
  convert v@(ValTag _)       = error $ "Cannot transform ValTag to new syntax: " ++ show (PP v)
  convert Unit          = New.Unit
  convert (Lit l)       = New.Lit l
  convert (Var v)       = New.Var v
  convert (Undefined t) = New.Undefined t

instance Convertible Exp New.Exp where
  convert (Program exts defs)  = New.Program exts (map convert defs)
  convert (Def name args body) = New.Def name args (convert body)
  {- NOTE: we assume Binding Pattern Simplification has been run
    v.0 <- pure <value>
    <non-var pat> <- pure v.0
    <rhs2>
  -}
  convert (EBind lhs1 (Var var) rhs1)
    | EBind (SReturn (Var var')) pat rhs2 <- rhs1
    , isn't _Var pat
    , var == var'
    = New.EBind (convert lhs1) (New.AsPat var (convert pat)) (convert rhs2)
  convert (EBind lhs (Var var) rhs)
    = New.EBind (convert lhs) (New.VarPat var) (convert rhs)
  convert (ECase scrut alts)
    | isn't _Var scrut   = error $ "Non-variable pattern in case scrutinee: " ++ show (PP scrut)
    | (Var var) <- scrut = New.ECase var (map convert alts)
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
  convert (New.Unit)  = Unit
  convert (New.Lit l) = Lit l
  convert (New.Var v) = Var v
  convert (New.Undefined t) = Undefined t

instance Convertible New.Exp Exp where
  convert (New.Program exts defs)  = Program exts (map convert defs)
  convert (New.Def name args body) = Def name args (convert body)
  convert e@(New.EBind lhs pat rhs)
    | (New.VarPat v)      <- pat = EBind (convert lhs) (Var v) (convert rhs)
    | (New.AsPat  v pat') <- pat -- condition
    , rhs' <- EBind (SReturn (Var v)) (convert pat') (convert rhs) -- helper
    = EBind (convert lhs) (Var v) rhs'
  convert e@(New.ECase scrut alts) = ECase (Var scrut) (map convert alts)
  convert (New.SApp f vars)        = SApp f $ map Var vars
  convert (New.SStore var)         = SStore (Var var)
  convert (New.SFetch ptr)         = SFetchI ptr Nothing
  convert (New.SUpdate ptr var)    = SUpdate ptr (Var var)
  convert (New.SReturn val)        = SReturn (convert val)
  convert (New.SBlock exp)         = SBlock (convert exp)
  convert (New.Alt cpat exp)       = Alt cpat (convert exp)

convertToNew :: Exp -> New.Exp
convertToNew = convert . nameEverything

nameEverything :: Exp -> Exp
nameEverything = nodeArgumentNaming
               . bindNormalisation
               . appArgumentNaming
               . bindNormalisation
               . fst . bindingPatternSimplification
               . bindNormalisation
               . fst . producerNameIntroduction
               . bindNormalisation

appArgumentNaming :: Exp -> Exp
appArgumentNaming e = fst . evalNameM e . cata alg $ e where
  alg :: ExpF (NameM Exp) -> NameM Exp
  alg e = case e of
    SAppF f args -> bindFunArgs f args
    expf -> fmap embed . sequence $ expf

  bindFunArgs :: Name -> [Val] -> NameM Exp
  bindFunArgs f args = do
    varArgs <- forM [1..length args] $ \_ ->
      Var <$> newArgName
    let g exp (arg, var) = EBind (SReturn arg) var exp
        boundApp = foldl g (SApp f varArgs) $ zip args varArgs
    pure $ SBlock boundApp

  newArgName :: NameM Name
  newArgName = deriveNewName "x"

-- NOTE: we can ssume tha Producer Name Introduction
-- & Binding Pattern Simplification has already been run
-- ConstTagNodes can only appear in SReturns
nodeArgumentNaming :: Exp -> Exp
nodeArgumentNaming e = fst . evalNameM e . cata alg $ e where
  alg :: ExpF (NameM Exp) -> NameM Exp
  alg e = case e of
    SReturnF (ConstTagNode tag args) -> bindNodeArgs tag args
    expf -> fmap embed . sequence $ expf

  bindNodeArgs :: Tag -> [Val] -> NameM Exp
  bindNodeArgs tag args = do
    varArgs <- forM [1..length args] $ \_ ->
      Var <$> newArgName
    let g exp (arg, var) = EBind (SReturn arg) var exp
        boundApp = foldl g (SReturn $ ConstTagNode tag varArgs) $ zip args varArgs
    pure $ SBlock boundApp

  newArgName :: NameM Name
  newArgName = deriveNewName "y"

testToNew :: IO ()
testToNew = do
  prog <- G.asExp <$> generate (resize 1 arbitrary :: Gen G.Prog)
  putStrLn $ show $ PP prog
  putStrLn "---------------"
  let prog'  = nameEverything prog
  putStrLn $ show $ PP prog'
  putStrLn "---------------"
  let prog'' = convertToNew prog
  putStrLn $ show $ PP prog''
