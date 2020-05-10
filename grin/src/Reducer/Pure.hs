{-# LANGUAGE LambdaCase, TupleSections, BangPatterns, OverloadedStrings, ConstraintKinds #-}
module Reducer.Pure
  ( EvalPlugin(..)
  , reduceFun
  ) where

import Text.Printf
import Text.PrettyPrint.ANSI.Leijen

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
  }
type GrinM a = ReaderT Context (StateT (StoreMap, Statistics) IO) a

lookupStore :: Int -> StoreMap -> RTVal
lookupStore i s = IntMap.findWithDefault (error $ printf "missing location: %d" i) i $ storeMap s

debug :: Bool
debug = False

evalSimpleExp :: Env -> SimpleExp -> GrinM RTVal
evalSimpleExp env s = do
  when debug $ do
    liftIO $ print s
    void $ liftIO $ getLine
  case s of
    SApp n a -> do
                let args = map (evalVal env) a
                    go a [] [] = a
                    go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                    go _ x y = error $ printf "invalid pattern for function: %s %s %s" n (prettyDebug x) (prettyDebug y)
                exts <- asks ctxExternals
                evalPrimOpMap <- asks (evalPluginPrimOp . ctxEvalPlugin)
                if isExternalName exts n
                  then do
                    let Just evalPrimOp = Map.lookup n evalPrimOpMap
                    liftIO $ evalPrimOp args
                  else do
                    Def _ vars body <- reader
                      ((Map.findWithDefault (error $ printf "unknown function: %s" n) n) . ctxProg)
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

reduceFun :: EvalPlugin -> Program -> Name -> IO (RTVal, Maybe Statistics)
reduceFun evalPrimOp (Program exts l) n = do
    (v, (_, s)) <- runStateT (runReaderT (evalExp mempty e) context) (emptyStore, emptyStatistics)
    pure (v, Just s)
  where
    context@(Context m _ _) = Context (Map.fromList [(n,d) | d@(Def n _ _) <- l]) exts evalPrimOp
    e = case Map.lookup n m of
          Nothing -> error $ printf "missing function: %s" n
          Just (Def _ [] a) -> a
          _ -> error $ printf "function %s has arguments" n
