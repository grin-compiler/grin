{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}
module Frontend.Lambda.FromDumpCore (codegenLambda) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.State
import Text.Printf
import qualified Data.ByteString.Char8 as BS8

-- GHC Dump
import qualified GhcDump_Ast as C

-- Lambda
import Frontend.Lambda.Syntax (Name)
import Frontend.Lambda.Syntax

type CG = StateT Env IO

data Env
  = Env
  { closureDefs :: [Def]
  , counter     :: Int
  , moduleName  :: String
  , dataCons    :: Set String
--  , dataArity   :: Map String Int
--  , varArity    :: Map String Int
  }

convertUnique :: C.Unique -> String
convertUnique (C.Unique c i) = c : show i

genName :: String -> C.Binder -> CG String
genName p b = do
  modName <- gets moduleName
  let name = BS8.unpack . C.binderName $ C.unBndr b
      qualName = modName ++ "." ++ name
  case C.unBndr b of
    C.Binder{..} -> do
      let C.IdInfo{..} = binderIdInfo
      when (binderIdDetails `elem` [C.DataConWorkId]) $ do
        modify $ \env@Env{..} -> env {dataCons = Set.insert (p ++ "-" ++ show idiArity ++ "-" ++ qualName) dataCons}
    _ -> pure ()
  pure qualName

isDataCon :: C.Binder -> Bool
isDataCon b = case C.unBndr b of
  C.Binder{..} -> binderIdDetails == C.DataConWorkId
  _ -> False

isDataConExt :: C.ExternalName -> Bool
isDataConExt C.ForeignCall = False
isDataConExt b = C.externalIdDetails b == C.DataConWorkId

{-
TODO - support these:
data Lit = MachNullAddr
         | MachLabel T_Text
         | LitInteger Integer
-}
convertLit :: C.Lit -> Lit
convertLit = \case
  C.MachInt     i -> LInt64 $ fromIntegral i
  C.MachInt64   i -> LInt64 $ fromIntegral i
  C.MachWord    w -> LWord64 $ fromIntegral w
  C.MachWord64  w -> LWord64 $ fromIntegral w
  C.MachFloat   f -> LFloat $ realToFrac f
  C.MachDouble  f -> LFloat $ realToFrac f
  C.MachStr     s -> LString s
  C.MachChar    c -> LChar c
  lit -> LError . show $ lit

visitAlt :: C.Alt -> CG Alt
visitAlt (C.Alt altCon argIds body) = do
  cpat <- case altCon of
    C.AltDataCon moduleName dataCon -> NodePat (maybe (error $ "missing data con module name: " ++ show dataCon) (\n -> BS8.unpack n ++ ".") moduleName ++ BS8.unpack dataCon) <$> mapM (genName "alt") argIds
    C.AltLit lit                    -> pure . LitPat $ convertLit lit
    C.AltDefault                    -> pure DefaultPat
  Alt cpat <$> visitExpr body

convertExtName :: C.ExternalName -> CG String
convertExtName = \case
  C.ExternalName {..} -> do
    let qualName = BS8.unpack (C.getModuleName externalModuleName) ++ "." ++ BS8.unpack externalName
    modify $ \env@Env{..} -> env {dataCons = Set.insert ("EVarGlobal" ++ "-" ++ show externalIdArity ++ "-" ++ qualName) dataCons}
    pure $ BS8.unpack (C.getModuleName externalModuleName) ++ "." ++ BS8.unpack externalName
  C.ForeignCall       -> pure "_foreignCall"

isTyBinder :: C.Binder -> Bool
isTyBinder = \case
  ((C.Bndr C.TyBinder{})) -> True
  _ -> False

visitExpr :: C.Expr -> CG Exp
visitExpr = \case
  C.EVar bndr
    | isTyBinder bndr   -> pure . Lit . LDummy $ "TyBinder"
    | isDataCon bndr    -> Con <$> genName "EVar data con" bndr <*> pure []
    | otherwise         -> Var <$> genName "EVar" bndr
  C.EVarGlobal extName
    | C.externalIsTyVar extName -> pure . Lit . LDummy $ "ExtTyBinder"
    | isDataConExt extName      -> Con <$> convertExtName extName <*> pure []
    | otherwise                 -> Var <$> convertExtName extName
  C.ELit lit            -> pure . Lit $ convertLit lit
  C.EApp fun arg        -> AppCore <$> visitExpr fun <*> visitExpr arg
  C.ETyLam bndr expr    -> visitExpr expr -- ignore
  C.ELam bndr expr      -> Lam <$> genName "ELam" bndr <*> visitExpr expr
  C.ELet binding expr   -> LetRec <$> mapM (\(n,e) -> (,) <$> genName "ELet" n <*> visitExpr e) binding <*> visitExpr expr

  C.EType t             -> pure . Lit . LDummy $ "EType"
  C.ECoercion           -> pure . Lit . LDummy $ "ECoercion"

  C.ECase expr resultId alts  -> do
    scrutName <- genName "ECase" resultId
    scrutExp <- visitExpr expr
    LetS [(scrutName, scrutExp)] . Case (Var scrutName) <$> mapM visitAlt alts

  expr -> error . printf "unsupported expr %s" $ show expr

visitTopBinder :: C.Module -> CG [Def]
visitTopBinder mod = mapM (\(bndr, _stats, expr) -> Def <$> genName "top binder" bndr <*> pure [] <*> visitExpr expr) $ C.moduleBindings mod

codegenLambda :: C.Module -> IO Program
codegenLambda mod = do
  let modName = BS8.unpack . C.getModuleName $ C.moduleName mod
  (defs, Env{..}) <- runStateT (visitTopBinder mod) (Env mempty 0 modName mempty)
  unless (Set.null dataCons) $ do
    printf "%s data constructors:\n%s" modName  (unlines . map show . Set.toList $ dataCons)
  pure . Program $ defs -- ++ closureDefs
