{-# LANGUAGE LambdaCase, TupleSections, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}
module Frontend.FromSTG (codegenGrin) where

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

-- Grin
import Grin

type CG = StateT Env IO

data Env
  = Env
  { dflags  :: DynFlags
  , defs    :: Map Name Def
  }

genName :: Id -> CG String
genName id = pprM id

pprM :: Outputable a => a -> CG String
pprM a = flip showPpr a <$> gets dflags

emit _ = pure () -- TODO

-- done
convertLit :: Literal -> CG Lit
convertLit = \case
  MachInt     i -> pure $ LInt64 $ fromIntegral i
  MachInt64   i -> pure $ LInt64 $ fromIntegral i
  MachWord    w -> pure $ LWord64 $ fromIntegral w
  MachWord64  w -> pure $ LWord64 $ fromIntegral w
  MachFloat   f -> pure $ LFloat $ realToFrac f
  MachDouble  f -> pure $ LFloat $ realToFrac f
  lit -> error . printf "unsupported literal %s" <$> pprM lit

-- done
visitArg' :: StgArg -> CG Val
visitArg' = \case
  StgVarArg id  -> Var <$> genName id
  StgLitArg lit -> Lit <$> convertLit lit

-- done
visitArg a = do
  v <- visitArg' a
  liftIO $ print v
  pure v

visitRhs :: Id -> StgRhs -> CG ()
visitRhs id rhs = case rhs of
  StgRhsCon _ dataCon args -> do
    name <- genName id
    liftIO $ putStrLn $ printf " * def (data) name: %s" name
    pure () -- TODO
  StgRhsClosure _ _ freeVars _ args body -> do
    name <- genName id
    freeVars_ <- mapM genName freeVars
    args_ <- mapM genName args
    liftIO $ putStrLn $ printf " * def (fun) name: %s free vars: %s args: %s" name (show freeVars_) (show args_)
    {-
      TODO:
        - add def to globals with the right argumentum list
        - generate the body
    -}
    visitExpr body
    pure ()

{-
  | Def         Name [Name] Exp
  -- Exp
  | EBind       SimpleExp LPat Exp
  | SBlock      Exp
  | ECase       Val [Alt]
  -- Simple Exp
  | SApp        Name [SimpleVal]
  | SReturn     Val
  | SStore      Val
  -- Alt
  | Alt CPat Exp

  -- only in eval
  | SFetchI     Name (Maybe Int) -- fetch a full node or a single node item in low level GRIN
  | SUpdate     Name Val

-}

{-
  TODO:
    - introduce def
    - build bind sequence
-}

visitTopBinding :: StgTopBinding -> CG ()
visitTopBinding = \case
  StgTopLifted    binding       -> visitBinding binding
  StgTopStringLit id byteString -> error "unsupported: StgTopStringLit"

visitBinding :: StgBinding -> CG ()
visitBinding = \case
  StgNonRec id stgRhs -> visitRhs id stgRhs
  StgRec bindings     -> mapM_ (uncurry visitRhs) bindings

visitExpr :: StgExpr -> CG Exp
visitExpr = \case
  -- app
  StgApp id args                  -> SApp <$> genName id <*> mapM visitArg args
  StgOpApp op args _ty            -> SApp <$> genOpName op <*> mapM visitArg args
  --StgConApp dataCon args _ty      -> ConstTagNode <$> genTag dataCon <*> mapM visitArg args >>= fmap SReturn
  -- return lit
  StgLit literal                  -> SReturn . Lit <$> convertLit literal
  -- bypass
  StgTick _tickish expr           -> visitExpr expr
  -- ???
  StgLet binding expr             -> visitBinding binding >> visitExpr expr -- TODO: generate local or global bind
  StgLetNoEscape binding expr     -> visitBinding binding >> visitExpr expr -- TODO: generate local or global bind
  -- case + bind
  StgCase expr resultId _ty alts  -> do
    -- visitExpr expr >> mapM_ visitAlt alts  -- TODO: construct case expression
    n <- genName resultId
    liftIO (putStrLn $ "StgCase result: " ++ n)
    visitExpr expr
  expr -> error . printf "unsupported expr %s" <$> pprM expr

genOpName :: StgOp -> CG String
genOpName = \case
  StgPrimOp op      -> pprM op -- TODO
  StgPrimCallOp op  -> pprM op -- TODO
  StgFCallOp op _   -> pprM op -- TODO

{-
data DataCon
  = MkData {
        dcName    :: Name,      -- This is the name of the *source data con*
                                -- (see "Note [Data Constructor Naming]" above)
        dcUnique :: Unique,     -- Cached from Name
        dcTag    :: ConTag,     -- ^ Tag, used for ordering 'DataCon's
-}
genTag :: DataCon -> CG Tag
genTag dataCon = Tag C <$> pprM dataCon

visitAlt :: StgAlt -> CG Alt
visitAlt (altCon, argIds, body) = do
  cpat <- case altCon of
    DataAlt dataCon -> NodePat <$> genTag dataCon <*> mapM genName argIds
    LitAlt  lit     -> LitPat <$> convertLit lit
    DEFAULT         -> pure DefaultPat
  Alt cpat <$> visitExpr body


{-
  top binding / binding / rhs
    expr
      arg / lit / alt

  method:
    - lambda lift STG
    - compile to grin according boquist phd
-}

codegenGrin :: DynFlags -> [StgTopBinding] -> IO Program
codegenGrin dflags stg = do
  let ppTopBinding :: StgTopBinding -> IO ()
      ppTopBinding = \case
        StgTopLifted    binding       -> putStrLn "StgTopLifted" >> ppBinding binding >> putStr "\n-----\n\n"
        StgTopStringLit id byteString -> error "unsupported: StgTopStringLit"

      ppBinding :: StgBinding -> IO ()
      ppBinding = \case
        StgNonRec id stgRhs -> putStrLn "StgNonRec" >> ppRhs id stgRhs
        StgRec bindings     -> putStrLn "StgRec" >> mapM_ (uncurry ppRhs) bindings

      ppRhs :: Id -> StgRhs -> IO ()
      ppRhs id rhs = putStrLn ("RHS " ++ showPpr dflags id) >> putStrLn (showPpr dflags rhs) >> putStr "\n"

  mapM_ ppTopBinding stg

  execStateT (mapM visitTopBinding stg) (Env dflags mempty)
  pure $ Program []



{-
  TODO:
    - generate top level functions
    - apply free variables when calling functions
    - handle lazyness
-}
