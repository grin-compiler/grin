{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}
{-# LANGUAGE Strict #-}
module Reducer.IO (reduceFun) where

import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.State
import Control.Monad.Reader

import Data.Vector.Mutable as Vector
import Data.IORef
import Control.Monad.RWS.Strict hiding (Alt)

import Reducer.PrimOps
import Grin

-- models computer memory
data IOStore = IOStore {
    sVector :: IOVector Val
  , sLast   :: IORef Int
  }

emptyStore1 :: IO IOStore
emptyStore1 = IOStore <$> new (10 * 1024 * 1024) <*> newIORef 0

-- models cpu registers
type Env = Map Name Val
type Prog = Map Name Def
type GrinS a = RWST Prog () IOStore IO a

getProg :: GrinS Prog
getProg = reader id

getStore :: GrinS IOStore
getStore = get

-- TODO: Resize
insertStore :: Val -> GrinS Int
insertStore x = do
  (IOStore v l) <- getStore
  lift $ do
    n <- readIORef l
    Vector.write v n x
    writeIORef l (n + 1)
    pure n

lookupStore :: Int -> GrinS Val
lookupStore n = do
  (IOStore v _) <- getStore
  lift $ do
    Vector.read v n

updateStore :: Int -> Val -> GrinS ()
updateStore n x = do
  (IOStore v _) <- getStore
  lift $ do
    Vector.write v n x

bindPatMany :: Env -> [Val] -> [LPat] -> Env
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env Undefined lpat) [] lpats
bindPatMany _ vals lpats = error $ "bindPatMany - pattern mismatch: " ++ show (vals, lpats)

bindPat :: Env -> Val -> LPat -> Env
bindPat env !val lpat = case lpat of
  Var n -> case val of
              ValTag{}  -> Map.insert n val env
              Unit      -> Map.insert n val env
              Lit{}     -> Map.insert n val env
              Loc{}     -> Map.insert n val env
              Undefined -> Map.insert n val env
              _ -> {-trace ("bindPat - illegal value: " ++ show val) $ -}Map.insert n val env -- WTF????
              _ -> error $ "bindPat - illegal value: " ++ show val
  ConstTagNode ptag pargs -> case val of
                  ConstTagNode vtag vargs | ptag == vtag -> bindPatMany env vargs pargs
                  _ -> error $ "bindPat - illegal value for ConstTagNode: " ++ show val
  VarTagNode varname pargs -> case val of
                  ConstTagNode vtag vargs -> bindPatMany (Map.insert varname (ValTag vtag) env) vargs pargs
                  _ -> error $ "bindPat - illegal value for ConstTagNode: " ++ show val
  Unit -> env
  _ -> error $ "bindPat - pattern mismatch" ++ show (val,lpat)

lookupEnv :: Name -> Env -> Val
lookupEnv n env = Map.findWithDefault (error $ "missing variable: " ++ n) n env

evalVal :: Env -> Val -> Val
evalVal env = \case
  v@Lit{}     -> v
  Var n       -> lookupEnv n env
  ConstTagNode t a -> ConstTagNode t $ map (evalVal env) a
  VarTagNode n a -> case lookupEnv n env of
                  Var n     -> VarTagNode n $ map (evalVal env) a
                  ValTag t  -> ConstTagNode t $ map (evalVal env) a
                  x -> error $ "evalVal - invalid VarTagNode tag: " ++ show x
  v@ValTag{}  -> v
  v@Unit      -> v
  v@Loc{}     -> v
  x -> error $ "evalVal: " ++ show x

pprint exp = trace (f exp) exp where
  f = \case
    EBind  a b _ -> unwords ["Bind", "{",show a,"} to {", show b, "}"]
    ECase  a _ -> unwords ["Case", show a]
    SBlock {} -> "Block"
    a -> show a


evalExp :: Env -> Exp -> GrinS Val
evalExp env exp = case {-pprint-} exp of
  EBind op pat exp -> evalSimpleExp env op >>= \v -> evalExp (bindPat env v pat) exp
  ECase v alts ->
    let defaultAlts = [exp | Alt DefaultPat exp <- alts]
        defaultAlt  = if Prelude.length defaultAlts > 1
                        then error "multiple default case alternative"
                        else Prelude.take 1 defaultAlts
    in case evalVal env v of
      ConstTagNode t l ->
                     let (vars,exp) = head $ [(b,exp) | Alt (NodePat a b) exp <- alts, a == t] ++ map ([],) defaultAlt ++ error ("evalExp - missing Case Node alternative for: " ++ show t)
                         go a [] [] = a
                         go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                         go _ x y = error $ "invalid pattern and constructor: " ++ show (t,x,y)
                     in  evalExp (go env vars l) exp
      ValTag t    -> evalExp env $ head $ [exp | Alt (TagPat a) exp <- alts, a == t] ++ defaultAlt ++ error ("evalExp - missing Case Tag alternative for: " ++ show t)
      Lit l       -> evalExp env $ head $ [exp | Alt (LitPat a) exp <- alts, a == l] ++ defaultAlt ++ error ("evalExp - missing Case Lit alternative for: " ++ show l)
      x -> error $ "evalExp - invalid Case dispatch value: " ++ show x
  exp -> evalSimpleExp env exp

evalSimpleExp :: Env -> SimpleExp -> GrinS Val
evalSimpleExp env = \case
  SApp n a -> do
              let args = map (evalVal env) a
                  go a [] [] = a
                  go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                  go _ x y = error $ "invalid pattern for function: " ++ show (n,x,y)
              if isPrimName n
                then evalPrimOp n args
                else do
                  Def _ vars body <- (Map.findWithDefault (error $ "unknown function: " ++ n) n) <$> getProg
                  evalExp (go env vars args) body
  SReturn v -> return $ evalVal env v
  SStore v -> do
              let v' = evalVal env v
              l <- insertStore v'
              -- modify' (\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) (s+1))
              return $ Loc l
  SFetchI n index -> case lookupEnv n env of
              Loc l -> selectNodeItem index <$> lookupStore l
              x -> error $ "evalSimpleExp - Fetch expected location, got: " ++ show x
--  | FetchI  Name Int -- fetch node component
  SUpdate n v -> do
              let v' = evalVal env v
              case lookupEnv n env of
                Loc l -> updateStore l v' >> return v'
                x -> error $ "evalSimpleExp - Update expected location, got: " ++ show x
  SBlock a -> evalExp env a
  x -> error $ "evalSimpleExp: " ++ show x

reduceFun :: Program -> Name -> IO Val
reduceFun (Program l) n = do
  store <- emptyStore1
  (val, _, _) <- runRWST (evalExp mempty e) m store
  return val
  where
    m = Map.fromList [(n,d) | d@(Def n _ _) <- l]
    e = case Map.lookup n m of
          Nothing -> error $ "missing function: " ++ n
          Just (Def _ [] a) -> a
          _ -> error $ "function " ++ n ++ " has arguments"
