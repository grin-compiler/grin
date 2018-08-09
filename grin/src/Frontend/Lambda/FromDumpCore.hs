{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}
module Frontend.Lambda.FromDumpCore (codegenLambda) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Text.Printf
import qualified Data.ByteString.Char8 as BS8

-- GHC Dump
import qualified GhcDump_Ast as C
import GhcDump.Pretty

-- Lambda
import Frontend.Lambda.Syntax (Name)
import Frontend.Lambda.Syntax

type CG = StateT Env IO

data Env
  = Env
  { closureDefs :: [Def]
  , counter     :: Int
  }

convertUnique :: C.Unique -> String
convertUnique (C.Unique c i) = c : show i

genName :: C.Binder -> CG String
genName b = do
  {-
  let C.BinderId u = C.binderId . C.unBndr $ b
  pure $ convertUnique u
  -}
  pure . show . pretty $ b
{-
TODO - support these:
data Lit = MachChar Char
         | MachStr BS.ByteString
         | MachNullAddr
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
  lit -> LError . show $ lit

visitAlt :: C.Alt -> CG Alt
visitAlt (C.Alt altCon argIds body) = do
  cpat <- case altCon of
    C.AltDataCon dataCon  -> NodePat (BS8.unpack dataCon) <$> mapM genName argIds
    C.AltLit lit          -> pure . LitPat $ convertLit lit
    C.AltDefault          -> pure DefaultPat
  Alt cpat <$> visitExpr body

{-
data ExternalName = ExternalName { externalModuleName :: !ModuleName
                                 , externalName :: !T_Text
                                 , externalUnique :: !Unique
                                 }
                  | ForeignCall
-}
convertExtName :: C.ExternalName -> String
convertExtName extName = show $ pretty extName
{-
convertExtName = \case
  C.ExternalName {..} -> BS8.unpack externalName --convertUnique externalUnique
  C.ForeignCall       -> "_foreignCall"
-}
visitExpr :: C.Expr -> CG Exp
visitExpr = \case
  C.EVar bndr           -> Var <$> genName bndr
  C.EVarGlobal extName  -> pure . Var $ convertExtName extName -- TODO: proper name conversion
  C.ELit lit            -> pure . Lit $ convertLit lit
  C.EApp fun arg        -> AppCore <$> visitExpr fun <*> visitExpr arg
  C.ETyLam bndr expr    -> Lam <$> genName bndr <*> visitExpr expr -- same as ELam
  C.ELam bndr expr      -> Lam <$> genName bndr <*> visitExpr expr
  C.ELet binding expr   -> LetRec <$> mapM (\(n,e) -> (,) <$> genName n <*> visitExpr e) binding <*> visitExpr expr

  C.EType t             -> pure . Lit . LError $ "EType"
  C.ECoercion           -> pure . Lit . LError $ "ECoercion"

  C.ECase expr resultId alts  -> do
    scrutName <- genName resultId
    scrutExp <- visitExpr expr
    LetS [(scrutName, scrutExp)] . Case (Var scrutName) <$> mapM visitAlt alts

  expr -> error . printf "unsupported expr %s" $ show expr

visitTopBinder :: C.Module -> CG [Def]
visitTopBinder mod = mapM (\(bndr, _stats, expr) -> Def <$> (modNamePrefix <$> genName bndr) <*> pure [] <*> visitExpr expr) $ C.moduleBindings mod
  where
    modName = BS8.unpack . C.getModuleName $ C.moduleName mod
    modNamePrefix x = x -- (modName ++ ".") ++ x

codegenLambda :: C.Module -> IO Program
codegenLambda mod = do
  let modName = BS8.unpack . C.getModuleName $ C.moduleName mod
  putStrLn $ modName
  (defs, Env{..}) <- runStateT (visitTopBinder mod) (Env mempty 0)
  pure . Program $ defs -- ++ closureDefs
