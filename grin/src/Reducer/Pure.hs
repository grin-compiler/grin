{-# LANGUAGE LambdaCase, TupleSections, BangPatterns, OverloadedStrings, ConstraintKinds #-}
module Reducer.Pure
  ( EvalPlugin(..)
  , ValueConstraints
  , reduceFun
  ) where

import Text.Printf
import Text.PrettyPrint.ANSI.Leijen

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.State
import Control.Monad.Reader
import Text.Printf

import Reducer.Base
import Reducer.PrimOps
import Grin.Grin
import Grin.Pretty

prettyDebug :: Pretty a => a -> String
prettyDebug = show . plain . pretty

-- models computer memory
data StoreMap v
  = StoreMap
  { storeMap  :: IntMap (RTVal v)
  , storeSize :: !Int
  }

emptyStore = StoreMap mempty 0

type ValueConstraints v = (Eq v, Show v, Pretty v)
data EvalPlugin v = EvalPlugin
  { evalPluginPrimOp  :: Name -> [Val] -> [RTVal v] -> IO (RTVal v)
  , evalPluginLiteral :: Lit -> v
  }

type Prog = Map Name Def
data Context v = Context
  { ctxProg       :: Prog
  , ctxExternals  :: [External]
  , ctxEvalPlugin :: EvalPlugin v
  }
type GrinM v a = ReaderT (Context v) (StateT (StoreMap v) IO) a

lookupStore :: Int -> StoreMap v -> (RTVal v)
lookupStore i s = IntMap.findWithDefault (error $ printf "missing location: %d" i) i $ storeMap s

debug :: Bool
debug = False

evalSimpleExp :: ValueConstraints v => Env v -> SimpleExp -> GrinM v (RTVal v)
evalSimpleExp env s = do
  when debug $ do
    liftIO $ print s
    void $ liftIO $ getLine
  case s of
    SApp n a -> do
                evalLit <- asks (evalPluginLiteral . ctxEvalPlugin)
                let args = map (evalVal evalLit env) a
                    go a [] [] = a
                    go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                    go _ x y = error $ printf "invalid pattern for function: %s %s %s" n (prettyDebug x) (prettyDebug y)
                exts <- asks ctxExternals
                evalPrimOp <- asks (evalPluginPrimOp . ctxEvalPlugin)
                if isExternalName exts n
                  then liftIO $ evalPrimOp n a args
                  else do
                    Def _ vars body <- reader
                      ((Map.findWithDefault (error $ printf "unknown function: %s" n) n) . ctxProg)
                    evalExp (go env vars args) body
    SReturn v -> do
                 evalLit <- asks (evalPluginLiteral . ctxEvalPlugin)
                 pure $ evalVal evalLit env v
    SStore v -> do
                l <- gets storeSize
                evalLit <- asks (evalPluginLiteral . ctxEvalPlugin)
                let v' = evalVal evalLit env v
                modify' (\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) (s+1))
                pure $ RT_Loc l
    SFetchI n index -> case lookupEnv n env of
                RT_Loc l -> gets $ (selectNodeItem index . lookupStore l)
                x -> error $ printf "evalSimpleExp - Fetch expected location, got: %s" (prettyDebug x)
  --  | FetchI  Name Int -- fetch node component
    SUpdate n v -> do
                evalLit <- asks (evalPluginLiteral . ctxEvalPlugin)
                let v' = evalVal evalLit env v
                case lookupEnv n env of
                  RT_Loc l -> get >>= \(StoreMap m _) -> case IntMap.member l m of
                              False -> error $ printf "evalSimpleExp - Update unknown location: %d" l
                              True  -> modify' (\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) s) >> pure RT_Unit
                  x -> error $ printf "evalSimpleExp - Update expected location, got: %s" (prettyDebug x)
    SBlock a -> evalExp env a

    e@ECase{} -> evalExp env e -- FIXME: this should not be here!!! please investigate.

    x -> error $ printf "invalid simple expression %s" (prettyDebug x)

evalExp :: ValueConstraints v => Env v -> Exp -> GrinM v (RTVal v)
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
    evalLit <- asks (evalPluginLiteral . ctxEvalPlugin)
    case evalVal evalLit env v of
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
      RT_Lit l    -> evalExp env $ head $ [exp | Alt (LitPat a) exp <- alts, evalLit a == l] ++ defaultAlt ++ error (printf "evalExp - missing Case Lit alternative for: %s" (show l))
      x -> error $ printf "evalExp - invalid Case dispatch value: %s" (prettyDebug x)
  exp -> evalSimpleExp env exp

reduceFun :: ValueConstraints v => EvalPlugin v -> Program -> Name -> IO (RTVal v)
reduceFun evalPrimOp (Program exts l) n = evalStateT (runReaderT (evalExp mempty e) context) emptyStore where
  context@(Context m _ _) = Context (Map.fromList [(n,d) | d@(Def n _ _) <- l]) exts evalPrimOp
  e = case Map.lookup n m of
        Nothing -> error $ printf "missing function: %s" n
        Just (Def _ [] a) -> a
        _ -> error $ printf "function %s has arguments" n
