{-# LANGUAGE LambdaCase #-}
module ReduceGrin (reduceFun) where

import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.State
import Control.Monad.Reader

import Grin

-- models computer memory
data StoreMap
  = StoreMap
  { storeMap  :: IntMap Val
  , storeSize :: !Int
  }

emptyStore = StoreMap mempty 0

-- models cpu registers
type Env = Map Name Val
type GrinM = ReaderT Prog (State StoreMap)

bindPatMany :: Env -> [Val] -> [LPat] -> Env
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env Undefined lpat) [] lpats
bindPatMany _ vals lpats = error $ "bindPatMany - pattern mismatch: " ++ show (vals, lpats)

bindPat :: Env -> Val -> LPat -> Env
bindPat env val lpat = case lpat of
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

lookupStore :: Int -> StoreMap -> Val
lookupStore i s = IntMap.findWithDefault (error $ "missing location: " ++ show i) i $ storeMap s

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

evalSimpleExp :: Env -> SimpleExp -> GrinM Val
evalSimpleExp env = \case
  SApp n a -> {-# SCC eSE_App #-}do
              let args = map (evalVal env) a
                  go a [] [] = a
                  go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                  go _ x y = error $ "invalid pattern for function: " ++ show (n,x,y)
              case n of
                "add" -> primAdd args
                "mul" -> primMul args
                "intPrint" -> primIntPrint args
                "intGT" -> primIntGT args
                "intAdd" -> primAdd args
                _ -> do
                  Def _ vars body <- reader $ Map.findWithDefault (error $ "unknown function: " ++ n) n
                  evalExp (go env vars args) body
  SReturn v -> {-# SCC eSE_Return #-}return $ evalVal env v
  SStore v -> {-# SCC eSE_Store #-}do
              l <- {-# SCC eSE_Store_size #-}gets storeSize
              let v' = evalVal env v
              modify' ({-# SCC eSE_Store_insert #-}\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) (s+1))
              return $ Loc l
  SFetch n -> {-# SCC eSE_Fetch #-}case lookupEnv n env of
              Loc l -> gets $ lookupStore l
              x -> error $ "evalSimpleExp - Fetch expected location, got: " ++ show x
--  | FetchI  Name Int -- fetch node component
  SUpdate n v -> {-# SCC eSE_Update #-}do
              let v' = evalVal env v
              case lookupEnv n env of
                Loc l -> get >>= \(StoreMap m _) -> case IntMap.member l m of
                            False -> error $ "evalSimpleExp - Update unknown location: " ++ show l
                            True  -> modify' (\(StoreMap m s) -> StoreMap (IntMap.insert l v' m) s) >> return Unit
                x -> error $ "evalSimpleExp - Update expected location, got: " ++ show x
  SBlock a -> {-# SCC eSE_Block #-}evalExp env a
  x -> error $ "evalSimpleExp: " ++ show x

evalExp :: Env -> Exp -> GrinM Val
evalExp env = \case
  EBind op pat exp -> evalSimpleExp env op >>= \v -> evalExp (bindPat env v pat) exp
  ECase v alts -> case evalVal env v of
    ConstTagNode t l ->
                   let (vars,exp) = head $ [(b,exp) | Alt (NodePat a b) exp <- alts, a == t] ++ error ("evalExp - missing Case Node alternative for: " ++ show t)
                       go a [] [] = a
                       go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                       go _ x y = error $ "invalid pattern and constructor: " ++ show (t,x,y)
                   in  evalExp (go env vars l) exp
    ValTag t    -> evalExp env $ head $ [exp | Alt (TagPat a) exp <- alts, a == t] ++ error ("evalExp - missing Case Tag alternative for: " ++ show t)
    Lit l       -> evalExp env $ head $ [exp | Alt (LitPat a) exp <- alts, a == l] ++ error ("evalExp - missing Case Lit alternative for: " ++ show l)
    x -> error $ "evalExp - invalid Case dispatch value: " ++ show x
  exp -> evalSimpleExp env exp

-- primitive functions
primIntGT [Lit (LFloat a), Lit (LFloat b)] = return $ ValTag $ Tag C (if a > b then "True" else "False") 0
primIntGT x = error $ "primIntGT - invalid arguments: " ++ show x

primIntPrint [Lit (LFloat a)] = return $ Lit $ LFloat $ a
primIntPrint x = error $ "primIntPrint - invalid arguments: " ++ show x

primAdd [Lit (LFloat a), Lit (LFloat b)] = return $ Lit $ LFloat $ a + b
primAdd x = error $ "primAdd - invalid arguments: " ++ show x

primMul [Lit (LFloat a), Lit (LFloat b)] = return $ Lit $ LFloat $ a * b
primMul x = error $ "primMul - invalid arguments: " ++ show x

reduce :: Exp -> Val
reduce e = evalState (runReaderT (evalExp mempty e) mempty) emptyStore

reduceFun :: [Def] -> Name -> Val
reduceFun l n = evalState (runReaderT (evalExp mempty e) m) emptyStore where
  m = Map.fromList [(n,d) | d@(Def n _ _) <- l]
  e = case Map.lookup n m of
        Nothing -> error $ "missing function: " ++ n
        Just (Def _ [] a) -> a
        _ -> error $ "function " ++ n ++ " has arguments"

sadd = SApp "add" [Lit $ LFloat 3, Lit $ LFloat 2]
test = sadd
test2 = EBind sadd (Var "a") $ SApp "mul" [Var "a", Var "a"]
