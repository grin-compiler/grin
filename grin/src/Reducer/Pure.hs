{-# LANGUAGE LambdaCase, TupleSections, BangPatterns, OverloadedStrings #-}
module Reducer.Pure (reduceFun) where

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
data StoreMap
  = StoreMap
  { storeMap  :: IntMap RTVal
  , storeSize :: !Int
  }

emptyStore = StoreMap mempty 0

type Prog = Map Name Def
type GrinM = ReaderT Prog (StateT StoreMap IO)

lookupStore :: Int -> StoreMap -> RTVal
lookupStore i s = IntMap.findWithDefault (error $ printf "missing location: %d" i) i $ storeMap s

debug :: Bool
debug = False

evalSimpleExp :: [External] -> Env -> SimpleExp -> GrinM RTVal
evalSimpleExp exts env s = do
  when debug $ do
    liftIO $ print s
    void $ liftIO $ getLine
  case s of
    SApp n a -> do
                let args = map (evalVal env) a
                    go a [] [] = a
                    go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                    go _ x y = error $ printf "invalid pattern for function: %s %s %s" n (prettyDebug x) (prettyDebug y)
                if isExternalName exts n
                  then evalPrimOp n a args
                  else do
                    Def _ vars body <- reader $ Map.findWithDefault (error $ printf "unknown function: %s" n) n
                    evalExp exts (go env vars args) body
    SReturn v -> pure $ evalVal env v
    SStore v -> do
                l <- gets storeSize
                let v' = evalVal env v
                modify' (\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) (s+1))
                pure $ RT_Loc l
    SFetchI n index -> case lookupEnv n env of
                RT_Loc l -> gets $ (selectNodeItem index . lookupStore l)
                x -> error $ printf "evalSimpleExp - Fetch expected location, got: %s" (prettyDebug x)
  --  | FetchI  Name Int -- fetch node component
    SUpdate n v -> do
                let v' = evalVal env v
                case lookupEnv n env of
                  RT_Loc l -> get >>= \(StoreMap m _) -> case IntMap.member l m of
                              False -> error $ printf "evalSimpleExp - Update unknown location: %d" l
                              True  -> modify' (\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) s) >> pure RT_Unit
                  x -> error $ printf "evalSimpleExp - Update expected location, got: %s" (prettyDebug x)
    SBlock a -> evalExp exts env a

    e@ECase{} -> evalExp exts env e -- FIXME: this should not be here!!! please investigate.

    x -> error $ printf "invalid simple expression %s" (prettyDebug x)

evalExp :: [External] -> Env -> Exp -> GrinM RTVal
evalExp exts env = \case
  EBind op pat exp -> do
    v <- evalSimpleExp exts env op
    when debug $ do
      liftIO $ putStrLn $ unwords [show pat,":=",show v]
    evalExp exts (bindPat env v pat) exp
  ECase v alts ->
    let defaultAlts = [exp | Alt DefaultPat exp <- alts]
        defaultAlt  = if length defaultAlts > 1
                        then error "multiple default case alternative"
                        else take 1 defaultAlts
    in case evalVal env v of
      RT_ConstTagNode t l ->
                     let (vars,exp) = head $ [(b,exp) | Alt (NodePat a b) exp <- alts, a == t] ++ map ([],) defaultAlt ++ error (printf "evalExp - missing Case Node alternative for: %s" (prettyDebug t))
                         go a [] [] = a
                         go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                         go _ x y = error $ printf "invalid pattern and constructor: %s %s %s" (prettyDebug t) (prettyDebug x) (prettyDebug y)
                     in  evalExp
                            exts
                            (case vars of -- TODO: Better error check: If not default then parameters must match
                              [] -> {-defualt-} env
                              _  -> go env vars l)
                            exp
      RT_ValTag t -> evalExp exts env $ head $ [exp | Alt (TagPat a) exp <- alts, a == t] ++ defaultAlt ++ error (printf "evalExp - missing Case Tag alternative for: %s" (prettyDebug t))
      RT_Lit l    -> evalExp exts env $ head $ [exp | Alt (LitPat a) exp <- alts, a == l] ++ defaultAlt ++ error (printf "evalExp - missing Case Lit alternative for: %s" (prettyDebug l))
      -- TODO: more helpful error message showing the scrutinee
      x -> error $ printf "evalExp - invalid Case dispatch value: %s" (prettyDebug x)
  exp -> evalSimpleExp exts env exp

reduceFun :: Program -> Name -> IO RTVal
reduceFun (Program exts l) n = evalStateT (runReaderT (evalExp exts mempty e) m) emptyStore where
  m = Map.fromList [(n,d) | d@(Def n _ _) <- l]
  e = case Map.lookup n m of
        Nothing -> error $ printf "missing function: %s" n
        Just (Def _ [] a) -> a
        _ -> error $ printf "function %s has arguments" n
