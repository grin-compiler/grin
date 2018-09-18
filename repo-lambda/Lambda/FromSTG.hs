{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}
module Frontend.Lambda.FromSTG (codegenLambda) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Text.Printf

-- GHC
import CoreSyn (AltCon(..))
import StgSyn
import Id
import qualified Name as GHC
import DynFlags
import Outputable
import Literal
import DataCon

-- Lambda
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

visitArg :: StgArg -> CG Atom
visitArg = \case
  StgVarArg id  -> Var <$> genName id
  StgLitArg lit -> Lit <$> convertLit lit

-- expr level
visitRhs :: Name -> StgRhs -> CG Exp
visitRhs name stgRhs = case stgRhs of
  StgRhsCon _ dataCon args -> Con <$> pprM (dataConName dataCon) <*> mapM visitArg args

  StgRhsClosure _ _ [] _ [] body -> visitExpr body

  StgRhsClosure _ _ freeVars _ args body -> do
    freeVarNames <- mapM genName freeVars
    argNames <- mapM genName args

    closureName <- uniq $ printf "closure-%s" name
    visitTopRhs closureName stgRhs >>= addDef

    pure . App closureName . map Var $ freeVarNames ++ argNames

visitBinding :: StgBinding -> CG (Exp -> Exp)
visitBinding = \case
  StgNonRec id stgRhs -> do
    name <- genName id
    exp <- visitRhs name stgRhs
    pure (Let [(name, exp)])

  StgRec bindings -> LetRec <$> mapM f bindings where
    f (id, stgRhs) = do
      name <- genName id
      exp <- visitRhs name stgRhs
      pure (name, exp)

visitExpr :: StgExpr -> CG Exp
visitExpr = \case
  StgApp id args                  -> App <$> genName id <*> mapM visitArg args
  StgOpApp op args _ty            -> App <$> genOpName op <*> mapM visitArg args
  StgConApp dataCon args _        -> Con <$> pprM (dataConName dataCon) <*> mapM visitArg args
  StgLit literal                  -> Lit <$> convertLit literal

  -- bypass
  StgTick _tickish expr           -> visitExpr expr

  StgLet binding expr             -> ($) <$> visitBinding binding <*> visitExpr expr
  StgLetNoEscape binding expr     -> ($) <$> visitBinding binding <*> visitExpr expr

  StgCase stgExpr resultId _ty alts  -> do
    scrutName <- genName resultId
    scrutExp <- visitExpr stgExpr
    LetS [(scrutName, scrutExp)] . Case (Var scrutName) <$> mapM visitAlt alts

  expr -> error . printf "unsupported expr %s" <$> pprM expr

genOpName :: StgOp -> CG String
genOpName = \case
  StgPrimOp op      -> pprM op -- TODO
  StgPrimCallOp op  -> pprM op -- TODO
  StgFCallOp op _   -> pprM op -- TODO

genTag :: DataCon -> CG Name
genTag dataCon = pprM dataCon

visitAlt :: StgAlt -> CG Alt
visitAlt (altCon, argIds, body) = do
  cpat <- case altCon of
    DataAlt dataCon -> NodePat <$> genTag dataCon <*> mapM genName argIds
    LitAlt  lit     -> LitPat <$> convertLit lit
    DEFAULT         -> pure DefaultPat
  Alt cpat <$> visitExpr body

-- top level
visitTopRhs :: Name -> StgRhs -> CG Def
visitTopRhs name = \case
  StgRhsCon _ dataCon args -> Def name [] <$> (Con <$> pprM (dataConName dataCon) <*> mapM visitArg args)

  StgRhsClosure _ _ [] _ [] body
   -> Def name [] <$> visitExpr body

  StgRhsClosure _ _ freeVars _ args body -> do
    freeVarNames <- mapM genName freeVars
    argNames <- mapM genName args
    Def name (freeVarNames ++ argNames) <$> visitExpr body

visitTopBinding :: StgTopBinding -> CG [Def]
visitTopBinding = \case
  StgTopLifted (StgNonRec id stgRhs) -> do
    name <- genName id
    def <- visitTopRhs name stgRhs
    pure [def]

  StgTopLifted (StgRec bindings) -> forM bindings $ \(id, stgRhs) -> do
    name <- genName id
    visitTopRhs name stgRhs

  StgTopStringLit id byteString -> do
    name <- genName id
    pure [Def name [] . Lit . LError $ show byteString]

codegenLambda :: DynFlags -> [StgTopBinding] -> IO Program
codegenLambda dflags stg = do
  (defs, Env{..}) <- runStateT (mapM visitTopBinding stg) (Env dflags mempty 0)
  pure . Program $ concat defs ++ closureDefs
