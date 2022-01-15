{-# LANGUAGE LambdaCase, TupleSections, BangPatterns, OverloadedStrings, ConstraintKinds #-}
module Reducer.Pure
  ( EvalPlugin(..)
  , reduceFun
  ) where

import Text.Printf
import Text.PrettyPrint.ANSI.Leijen

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List
import qualified Data.Text as Text
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.LibFFI
import System.Posix.DynamicLinker
import qualified System.Info as Info

import Reducer.Base
import Reducer.PrimOps
import Grin.Grin
import Grin.Pretty

prettyDebug :: Pretty a => a -> String
prettyDebug = show . plain . pretty

-- models computer memory
data StoreMap
  = StoreMap
  { storeMap      :: !(IntMap RTVal)
  , storeSize     :: !Int
  }

emptyStore = StoreMap mempty 0

newtype EvalPlugin = EvalPlugin
  { evalPluginPrimOp  :: Map Name ([RTVal] -> IO RTVal)
  }

type Prog = Map Name Def
data Context = Context
  { ctxProg       :: Prog
  , ctxExternals  :: [External]
  , ctxEvalPlugin :: EvalPlugin
  , ctxForeign    :: Map Name (FunPtr ())
  }
type GrinM a = ReaderT Context (StateT (StoreMap, Statistics) IO) a

lookupStore :: Int -> StoreMap -> RTVal
lookupStore i s = IntMap.findWithDefault (error $ printf "missing location: %d" i) i $ storeMap s

debug :: Bool
debug = False

toFFIArg :: Ty -> RTVal -> GrinM Arg
toFFIArg (TySimple T_Int64) (RT_Lit (LInt64 x)) = pure $ argInt64 x
toFFIArg (TySimple T_Word64) (RT_Lit (LWord64 x)) = pure $ argWord64 x
toFFIArg (TySimple T_Float) (RT_Lit (LFloat x)) = pure $ argCFloat $ CFloat x
toFFIArg (TySimple T_Bool) (RT_Lit (LBool x)) = pure $ argInt $ if x then 1 else 0
toFFIArg (TySimple T_String) (RT_Lit (LString x)) = pure $ argString $ Text.unpack x
toFFIArg (TySimple T_Char) (RT_Lit (LChar x)) = pure $ argCChar $ castCharToCChar x
toFFIArg ty arg = throwError $ userError
    $ printf "Unexpected argument type or value in FFI call: %s :: %s" (show arg) (show ty)

toFFIRet :: Ty -> GrinM (RetType RTVal)
toFFIRet (TySimple T_Int64) = pure $ withRetType (pure . RT_Lit . LInt64) retInt64
toFFIRet (TySimple T_Word64) = pure $ withRetType (pure . RT_Lit . LWord64) retWord64
toFFIRet (TySimple T_Float) = pure $ withRetType (pure . RT_Lit . LFloat . (\case CFloat x -> x)) retCFloat
toFFIRet (TySimple T_Bool) = pure $ withRetType (pure . RT_Lit . LBool . (/= 0)) retCInt
toFFIRet (TySimple T_String) = pure $ withRetType (pure . RT_Lit . LString . Text.pack) retString
toFFIRet (TySimple T_Char) = pure $ withRetType (pure . RT_Lit . LChar . castCCharToChar) retCChar
toFFIRet (TySimple T_Unit) = pure $ withRetType (const $ pure RT_Unit) retVoid
toFFIRet ty = throwError $ userError
    $ printf "Unexpected return type in FFI call: %s" $ show ty

evalSimpleExp :: Env -> SimpleExp -> GrinM RTVal
evalSimpleExp env s = do
  when debug $ do
    liftIO $ print s
    void $ liftIO getLine
  case s of
    SApp n a -> do
                let args = map (evalVal env) a
                    go a [] [] = a
                    go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                    go _ x y =
                      error $ printf
                        "invalid pattern for function: %s %s %s"
                        n
                        (prettyDebug x)
                        (prettyDebug y)
                exts <- asks ctxExternals
                evalPrimOpMap <- asks (evalPluginPrimOp . ctxEvalPlugin)
                let extm = find (\e -> eName e == n) exts
                case extm of
                  Just (External _ retTy argsTy _ FFI _) -> do
                    ffiArgs <- zipWithM toFFIArg argsTy args
                    ffiRet <- toFFIRet retTy
                    extFns <- asks ctxForeign
                    let fn = fromMaybe
                          (error $ printf "Missing foreign function: %s" n)
                          (Map.lookup n extFns)
                    liftIO $ callFFI fn ffiRet ffiArgs
                  Just _ -> do
                    let evalPrimOp = Map.findWithDefault
                          (error $ printf "undefined primop: %s" n)
                          n
                          evalPrimOpMap
                    liftIO $ evalPrimOp args
                  Nothing -> do
                    Def _ vars body <- reader
                      $ Map.findWithDefault (error $ printf "unknown function: %s" n) n . ctxProg
                    evalExp (go env vars args) body
    SReturn v -> pure $ evalVal env v
    SStore v -> do
                l <- gets (storeSize . fst)
                let v' = evalVal env v
                modify' (\(StoreMap m s, Statistics f u) ->
                            ( StoreMap (IntMap.insert l v' m) (s+1)
                            , Statistics (IntMap.insert l 0 f) (IntMap.insert l 0 u)
                            ))
                pure $ RT_Loc l
    SFetchI n index -> case lookupEnv n env of
                RT_Loc l -> do
                  modify' (\(heap, Statistics f u) ->
                              (heap, Statistics (IntMap.adjust (+1) l f) u))
                  gets $ (selectNodeItem index . lookupStore l . fst)
                x -> error $ printf "evalSimpleExp - Fetch expected location, got: %s" (prettyDebug x)
  --  | FetchI  Name Int -- fetch node component
    SUpdate n v -> do
                let v' = evalVal env v
                case lookupEnv n env of
                  RT_Loc l -> do
                    (StoreMap m _, _) <- get
                    case IntMap.member l m of
                      False -> error $ printf "evalSimpleExp - Update unknown location: %d" l
                      True -> do
                        modify' (\(StoreMap m s, Statistics f u) ->
                          (StoreMap (IntMap.insert l v' m) s, Statistics f (IntMap.adjust (+1) l u)))
                        pure RT_Unit
                  x -> error $ printf "evalSimpleExp - Update expected location, got: %s" (prettyDebug x)
    SBlock a -> evalExp env a

    e@ECase{} -> evalExp env e -- FIXME: this should not be here!!! please investigate.

    x -> error $ printf "invalid simple expression %s" (prettyDebug x)

evalExp :: Env -> Exp -> GrinM RTVal
evalExp env = \case
  EBind op pat exp -> do
    v <- evalSimpleExp env op
    when debug $ do
      liftIO $ putStrLn $ unwords [show pat,":=",show v]
    evalExp (bindPat env v pat) exp
  ECase v alts -> do
    let defaultAlts = [exp | Alt DefaultPat exp <- alts]
        defaultAlt  = if length defaultAlts > 1
                        then error "multiple default case alternative"
                        else take 1 defaultAlts
    case evalVal env v of
      RT_ConstTagNode t l ->
                     let (vars,exp) = head $ [(b,exp) | Alt (NodePat a b) exp <- alts, a == t] ++ map ([],) defaultAlt ++ error (printf "evalExp - missing Case Node alternative for: %s" (prettyDebug t))
                         go a [] [] = a
                         go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                         go _ x y = error $ printf "invalid pattern and constructor: %s %s %s" (prettyDebug t) (prettyDebug x) (prettyDebug y)
                     in  evalExp
                            (case vars of -- TODO: Better error check: If not default then parameters must match
                              [] -> {-defualt-} env
                              _  -> go env vars l)
                            exp
      RT_ValTag t -> evalExp env $ head $ [exp | Alt (TagPat a) exp <- alts, a == t] ++ defaultAlt ++ error (printf "evalExp - missing Case Tag alternative for: %s" (prettyDebug t))
      RT_Lit l    -> evalExp env $ head $ [exp | Alt (LitPat a) exp <- alts, a == l] ++ defaultAlt ++ error (printf "evalExp - missing Case Lit alternative for: %s" (show l))
      x -> error $ printf "evalExp - invalid Case dispatch value: %s" (prettyDebug x)
  exp -> evalSimpleExp env exp

createForeignFns :: [External] -> Map Name (FunPtr ()) -> IO (Map Name (FunPtr ()))
createForeignFns [] acc = pure acc
createForeignFns exts@(ext : _) acc
  | eKind ext == PrimOp = createForeignFns (tail exts) acc
  | otherwise = case find (\(os, _) -> toString os == Info.os) (eLibs ext) of
    Nothing -> do
        ptr <- dlsym Default $ Text.unpack $ unNM $ eName ext
        createForeignFns (tail exts) $ Map.insert (eName ext) ptr acc
    Just (os, lib) -> do
        let (extWithLib, extNoLib) =
              partition
                (\e ->
                  find
                    (\(os, l) -> toString os == Info.os)
                    (eLibs e)
                  == Just (os, lib))
                exts
        acc' <- withDL
            (Text.unpack lib)
            [RTLD_LAZY]
            (\dl -> foldlM (\a e -> do
                ptr <- dlsym dl (Text.unpack $ unNM $ eName e)
                pure $ Map.insert (eName e) ptr acc) acc extWithLib)
        createForeignFns extNoLib acc'
  where
    toString :: OS -> String
    toString = \case
      Darwin -> "darwin"
      FreeBSD -> "freebsd"
      Linux -> "linux"
      Android -> "linux-android"
      MinGW -> "mingw32"
      Win -> "mingw32"
      NetBSD -> "netbsd"
      OpenBSD -> "openbsd"

reduceFun :: EvalPlugin -> Program -> Name -> IO (RTVal, Maybe Statistics)
reduceFun evalPrimOp (Program exts l) n = do
    ffiFns <- createForeignFns exts mempty
    let context@(Context m _ _ _) = Context (Map.fromList [(n,d) | d@(Def n _ _) <- l]) exts evalPrimOp ffiFns
        e = case Map.lookup n m of
          Nothing -> error $ printf "missing function: %s" n
          Just (Def _ [] a) -> a
          _ -> error $ printf "function %s has arguments" n
    (v, (_, s)) <- runStateT (runReaderT (evalExp mempty e) context) (emptyStore, emptyStatistics)
    pure (v, Just s)
