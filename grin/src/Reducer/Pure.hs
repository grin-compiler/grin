{-# LANGUAGE LambdaCase #-}
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

import Reducer.PrimOps
import Grin
import Pretty

prettyDebug :: Pretty a => a -> String
prettyDebug = show . plain . pretty

-- models computer memory
data StoreMap
  = StoreMap
  { storeMap  :: IntMap Val
  , storeSize :: !Int
  }

emptyStore = StoreMap mempty 0

-- models cpu registers
type Env = Map Name Val
type Prog = Map Name Def
type GrinM = ReaderT Prog (State StoreMap)

bindPatMany :: Env -> [Val] -> [LPat] -> Env
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env Undefined lpat) [] lpats
bindPatMany _ vals lpats = error $ printf "bindPatMany - pattern mismatch: %s %s" (prettyDebug vals) (prettyDebug lpats)

bindPat :: Env -> Val -> LPat -> Env
bindPat env val lpat = case lpat of
  Var n -> Map.insert n val env
  ConstTagNode ptag pargs   | ConstTagNode vtag vargs <- val, ptag == vtag -> bindPatMany env vargs pargs
  VarTagNode varname pargs  | ConstTagNode vtag vargs <- val               -> bindPatMany (Map.insert varname (ValTag vtag) env) vargs pargs
  Unit -> env
  _ -> error $ printf "bindPat - pattern mismatch %s %s" (prettyDebug val) (prettyDebug lpat)

lookupEnv :: Name -> Env -> Val
lookupEnv n env = Map.findWithDefault (error $ printf "missing variable: %s" n) n env

lookupStore :: Int -> StoreMap -> Val
lookupStore i s = IntMap.findWithDefault (error $ printf "missing location: %d" i) i $ storeMap s

evalVal :: Env -> Val -> Val
evalVal env = \case
  v@Lit{}     -> v
  Var n       -> lookupEnv n env
  ConstTagNode t a -> ConstTagNode t $ map (evalVal env) a
  VarTagNode n a -> case lookupEnv n env of
                  -- NOTE: must be impossible (I guess)
                  -- Var n     -> VarTagNode n $ map (evalVal env) a
                  ValTag t  -> ConstTagNode t $ map (evalVal env) a
                  x -> error $ printf "evalVal - invalid VarTagNode tag: %s" (prettyDebug x)
  v@ValTag{}  -> v
  v@Unit      -> v
  v@Loc{}     -> v
  x -> error $ printf "evalVal: %s" (prettyDebug x)

evalSimpleExp :: Env -> SimpleExp -> GrinM Val
evalSimpleExp env = \case
  SApp n a -> do
              let args = map (evalVal env) a
                  go a [] [] = a
                  go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                  go _ x y = error $ printf "invalid pattern for function: %s %s %s" n (prettyDebug x) (prettyDebug y)
              if isPrimName n
                then evalPrimOp n args
                else do
                  Def _ vars body <- reader $ Map.findWithDefault (error $ printf "unknown function: %s" n) n
                  evalExp (go env vars args) body
  SReturn v -> return $ evalVal env v
  SStore v -> do
              l <- gets storeSize
              let v' = evalVal env v
              modify' (\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) (s+1))
              return $ Loc l
  SFetchI n index -> case lookupEnv n env of
              Loc l -> gets $ (selectNodeItem index . lookupStore l)
              x -> error $ printf "evalSimpleExp - Fetch expected location, got: %s" (prettyDebug x)
--  | FetchI  Name Int -- fetch node component
  SUpdate n v -> do
              let v' = evalVal env v
              case lookupEnv n env of
                Loc l -> get >>= \(StoreMap m _) -> case IntMap.member l m of
                            False -> error $ printf "evalSimpleExp - Update unknown location: %d" l
                            True  -> modify' (\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) s) >> return Unit
                x -> error $ printf "evalSimpleExp - Update expected location, got: %s" (prettyDebug x)
  SBlock a -> evalExp env a

  e@ECase{} -> evalExp env e -- FIXME: this should not be here!!! please investigate.

  x -> error $ printf "invalid simple expression %s" (prettyDebug x)

evalExp :: Env -> Exp -> GrinM Val
evalExp env = \case
  EBind op pat exp -> evalSimpleExp env op >>= \v -> evalExp (bindPat env v pat) exp
  ECase v alts -> case evalVal env v of
    ConstTagNode t l ->
                   let (vars,exp) = head $ [(b,exp) | Alt (NodePat a b) exp <- alts, a == t] ++ error (printf "evalExp - missing Case Node alternative for: %s" (prettyDebug t))
                       go a [] [] = a
                       go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                       go _ x y = error $ printf "invalid pattern and constructor: %s %s %s" (prettyDebug t) (prettyDebug x) (prettyDebug y)
                   in  evalExp (go env vars l) exp
    ValTag t    -> evalExp env $ head $ [exp | Alt (TagPat a) exp <- alts, a == t] ++ error (printf "evalExp - missing Case Tag alternative for: %s" (prettyDebug t))
    Lit l       -> evalExp env $ head $ [exp | Alt (LitPat a) exp <- alts, a == l] ++ error (printf "evalExp - missing Case Lit alternative for: %s" (prettyDebug l))
    x -> error $ printf "evalExp - invalid Case dispatch value: %s" (prettyDebug x)
  exp -> evalSimpleExp env exp

reduceFun :: Program -> Name -> Val
reduceFun (Program l) n = evalState (runReaderT (evalExp mempty e) m) emptyStore where
  m = Map.fromList [(n,d) | d@(Def n _ _) <- l]
  e = case Map.lookup n m of
        Nothing -> error $ printf "missing function: %s" n
        Just (Def _ [] a) -> a
        _ -> error $ printf "function %s has arguments" n
