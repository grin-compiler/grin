module Main where

import Control.Monad.Trans
import Control.Monad.Reader
import Data.List
import System.FilePath
import System.Directory
import System.IO

import qualified Agda.Main as M
import Agda.Compiler.Backend
import Agda.Interaction.Options
import Agda.Syntax.Common
import Agda.Syntax.Literal
import Agda.Syntax.Treeless as AT
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Monad

import Agda.Utils.Maybe
import Agda.Compiler.Common
import Agda.Compiler.Treeless.NormalizeNames


import Frontend.Lambda.Syntax as GL
import Frontend.Lambda.Pretty as GP
import qualified Text.PrettyPrint.ANSI.Leijen as PP

backend = Backend backend'

backend' :: Backend' Bool () () () (Maybe GL.Exp)
backend' = Backend'
  { backendName = "Grin"
  , backendVersion = Just "ZuriHac"
  , options = False
  , commandLineFlags = grinCommandLineFlags
  , isEnabled = id
  , preCompile = \_ -> return ()
  , postCompile = \_ _ _ -> return ()
  , preModule = \_ _ _ -> return $ Recompile ()
  , postModule = grinPostModule
  , compileDef = grinCompileDef
  , scopeCheckingSuffices = False
  }

main :: IO ()
main = M.runAgda [backend]

type GM = ReaderT GEnv TCM

data GEnv = GEnv
  { env :: [String]
  , freshNames :: [String]
  }

grinCommandLineFlags :: [OptDescr (Flag Bool)]
grinCommandLineFlags =
    [ Option []     ["grin"] (NoArg compileGrinFlag) "compile program using the Grin backend"
    ]
  where
    compileGrinFlag o      = return $ True

grinCompileDef :: () -> () -> Definition -> TCM (Maybe GL.Exp)
grinCompileDef env modEnv def = do
  case theDef def of
    Function{} -> do
      caseMaybeM (toTreeless $ defName def) (pure Nothing) $ \treeless -> do
        treeless <- normalizeNames treeless
        reportSDoc "compile.grin" 15 $ text "Treeless: " <+> pretty treeless
        grin <- runReaderT (toGrin treeless) initialEnv
        n' <- qnameToG $ defName def
        return $ Just $ GL.Def n' [] grin
    _ -> return Nothing
  where initialEnv = GEnv [] ([[c] | c <- ['a'..'z']] ++ [ "z" ++ show i | i <- [0..]])

extendEnv :: Int -> ([String] -> GM GL.Exp) -> GM GL.Exp
extendEnv n f = do
  (ns1,ns2) <- splitAt n <$> asks freshNames
  local (\e -> e { freshNames = ns2, env = ns1 ++ (env e) }) (f ns1)

toGrin :: AT.TTerm -> GM GL.Exp
toGrin t = case t of
  AT.TLam t -> extendEnv 1 $ \[nm] ->
      GL.Lam nm <$> toGrin t
  AT.TApp (AT.TDef n) ts -> GL.App <$> lift (qnameToG n) <*> traverse toGrin ts
  AT.TApp (AT.TCon n) ts -> GL.Con <$> lift (qnameToG n) <*> traverse toGrin ts
  AT.TApp t ts -> foldl GL.AppCore <$> (toGrin t) <*> (traverse toGrin ts)
  AT.TCon n -> GL.Con <$> lift(qnameToG n) <*> pure []
  AT.TLit l -> pure $ GL.Lit $ litToGrin l
  AT.TVar i -> GL.Var . (!! i) <$> asks env
  AT.TLet t1 t2 -> do
    t1' <- toGrin t1
    extendEnv 1 $ \[nm] -> do
      GL.Let [(nm, t1')] <$> toGrin t2
  AT.TCase sc ct def alts -> do
    sc' <- toGrin $ AT.TVar sc
    alts' <- traverse altToGrin alts
    alts' <- if AT.isUnreachable def then return alts' else do
      def' <- toGrin def
      return $ alts' ++ [GL.Alt GL.DefaultPat def']
    return $ GL.Case sc' alts'
  _ -> error $ show t

  where
    litToGrin l = case l of
      LitNat _ n -> GL.LInt64 $ fromInteger n
      _ -> undefined

    altToGrin a = case a of
      AT.TACon {aCon = n, aArity = ar, aBody = b} ->
        extendEnv ar $ \nms -> do
          n' <- lift $ qnameToG n
          GL.Alt (GL.NodePat n' nms) <$> toGrin b


qnameToG :: QName -> TCM String
qnameToG q = do
  modNm <- topLevelModuleName (qnameModule q)

  let locName = intercalate "_" $ map show $ either id id $ unqualifyQ modNm q
      modParts = intercalate "." $ map show $ mnameToList modNm
      identName = locName ++ "_" ++ show idnum

  return $ modParts ++ "." ++ identName
  where
    idnum = let (NameId x _) = nameId $ qnameName q in x

-- | Returns the names inside a QName, with the module prefix stripped away.
-- If the name is not module-qualified, returns the full name as left. (TODO investigate when this happens)
unqualifyQ :: ModuleName -> QName -> Either [AT.Name] [AT.Name]
unqualifyQ modNm qnm =
  case stripPrefix modNs qnms of
    -- not sure when the name doesn't have a module prefix... just force generation of a name for the time being
    Nothing -> Left qnms
    Just nm -> Right nm
  where
    modNs = mnameToList modNm
    qnms = qnameToList qnm


grinPostModule :: () -> () -> IsMain -> ModuleName -> [Maybe GL.Exp] -> TCM ()
grinPostModule _ _ _ modNm defs = do
  defs <- (return . GL.Program) $ catMaybes defs
  outFile <- grinPath modNm
--  let doc = PP.renderPretty 0.4 500 (PP.pretty defs)
  _ <- liftIO $ do
    handle <- openFile outFile WriteMode
    PP.hPutDoc handle (PP.plain $ PP.pretty defs)
    hClose handle
  return ()

grinPath :: ModuleName -> TCM FilePath
grinPath mName = do
  mdir <- compileDir
  let fp = mdir </> replaceExtension fp' "lam"
  liftIO $ createDirectoryIfMissing True (takeDirectory fp)
  return fp
   where
    fp' = foldl1 (</>) $ map show $ mnameToList mName

