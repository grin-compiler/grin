{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}
module Frontend.Lambda.FromCore (codegenLambda) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Text.Printf

-- GHC
import CoreSyn (CoreBind, CoreAlt, CoreExpr)
import PprCore
import qualified CoreSyn as C
import Id
import qualified Name as GHC
import DynFlags
import Outputable
import Literal
import DataCon

-- Lambda
import Frontend.Lambda.Syntax (Name)
import Frontend.Lambda.Syntax

type CG = StateT Env IO

data Env
  = Env
  { dflags      :: DynFlags
  , closureDefs :: [Def]
  , counter     :: Int
  }

addDef :: Def -> CG ()
addDef def = modify' $ \env@Env{..} -> env {closureDefs = def:closureDefs}

uniq :: Name -> CG Name
uniq name = state (\env@Env{..} -> (printf "%s.%d" name counter, env {counter = succ counter}))

genName :: Id -> CG String
genName id = do
  s <- pprM id
  pure s
  --pure $ printf "%s{%d,%d}" s (idArity id) (idCallArity id)

pprM :: Outputable a => a -> CG String
pprM a = flip showPpr a <$> gets dflags

convertLit :: Literal -> CG Lit
convertLit = \case
  MachInt     i -> pure $ LInt64 $ fromIntegral i
  MachInt64   i -> pure $ LInt64 $ fromIntegral i
  MachWord    w -> pure $ LWord64 $ fromIntegral w
  MachWord64  w -> pure $ LWord64 $ fromIntegral w
  MachFloat   f -> pure $ LFloat $ realToFrac f
  MachDouble  f -> pure $ LFloat $ realToFrac f
  lit -> LError <$> pprM lit

genTag :: DataCon -> CG Name
genTag dataCon = pprM dataCon

visitBinding :: CoreBind -> CG (Exp -> Exp)
visitBinding = \case
  C.NonRec id expr -> do
    name <- genName id
    exp <- visitExpr expr
    pure (Let [(name, exp)])

  C.Rec bindings -> LetRec <$> mapM f bindings where
    f (id, expr) = do
      name <- genName id
      exp <- visitExpr expr
      pure (name, exp)

visitAlt :: CoreAlt -> CG Alt
visitAlt (altCon, argIds, body) = do
  cpat <- case altCon of
    C.DataAlt dataCon -> NodePat <$> genTag dataCon <*> mapM genName argIds
    C.LitAlt  lit     -> LitPat <$> convertLit lit
    C.DEFAULT         -> pure DefaultPat
  Alt cpat <$> visitExpr body

{-
data Expr b
*  = Var   Id
*  | Lit   Literal
*  | App   (Expr b) (Arg b)
*  | Lam   b (Expr b)
*  | Let   (Bind b) (Expr b)
*  | Case  (Expr b) b Type [Alt b]       -- See #case_invariants#
*  | Cast  (Expr b) Coercion
*  | Tick  (Tickish Id) (Expr b)
*  | Type  Type
*  | Coercion Coercion

type Alt b = (AltCon, [b], Expr b)

data Bind b = NonRec b (Expr b)
            | Rec [(b, (Expr b))]
-}

visitExpr :: CoreExpr -> CG Exp
visitExpr = \case
  C.Var id              -> Var <$> genName id
  C.Lit lit             -> Lit <$> convertLit lit
  C.App fun arg         -> AppCore <$> visitExpr fun <*> visitExpr arg
  C.Lam id expr         -> Lam <$> genName id <*> visitExpr expr
  C.Let binding expr    -> ($) <$> visitBinding binding <*> visitExpr expr
  C.Tick _tickish expr  -> visitExpr expr
  C.Cast expr _         -> visitExpr expr
  C.Type t              -> Lit . LError <$> pprM t
  C.Coercion c          -> Lit . LError <$> pprM c

  C.Case expr resultId _ty alts  -> do
    scrutName <- genName resultId
    scrutExp <- visitExpr expr
    LetS [(scrutName, scrutExp)] . Case (Var scrutName) <$> mapM visitAlt alts

  expr -> error . printf "unsupported expr %s" <$> pprM expr

codegenLambda :: DynFlags -> [CoreBind] -> IO Program
codegenLambda dflags binds = do
  (defs, Env{..}) <- runStateT (mapM visitBinding binds) (Env dflags mempty 0)
  pure . Program $ {-map (flip ($) (Lit $ LError "top-level-bind-end")) $ concat -} map (flip ($) (Lit $ LError "top-level-bind-end")) defs -- ++ closureDefs
