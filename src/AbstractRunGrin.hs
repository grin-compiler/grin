{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractRunGrin where

import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.State
import Control.Monad.Reader
import Text.Printf

import Grin

type RTVal = Set Val

data Computer
  = Computer
  { storeMap  :: IntMap RTVal   -- models the computer memory
  , envMap    :: Map Name RTVal -- models the CPU registers
  }

emptyComputer = Computer mempty mempty

type GrinM = ReaderT Prog (State Computer)

{-
bindPatMany :: Env -> [RTVal] -> [LPat] -> Env
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env (Set.singleton Undefined) lpat) [] lpats
bindPatMany _ vals lpats = error $ "bindPatMany - pattern mismatch: " ++ show (vals, lpats)
-}
bindPat :: RTVal -> LPat -> GrinM ()
bindPat val lpat = case lpat of
  Var n -> modify' (\computer@Computer{..} -> computer {envMap = Map.insertWith mappend n val envMap})
{-
  ConstTagNode ptag pargs   | ConstTagNode vtag vargs <- val, ptag == vtag -> bindPatMany env vargs pargs
  VarTagNode varname pargs  | ConstTagNode vtag vargs <- val               -> bindPatMany (Map.insert varname (ValTag vtag) env) vargs pargs
-}
  Unit -> pure ()
  _ -> fail $ "bindPat - pattern mismatch" ++ show (val,lpat)


lookupEnv :: Name -> GrinM RTVal
lookupEnv n = Map.findWithDefault (error $ "missing variable: " ++ n) n <$> gets envMap

lookupStore :: Int -> GrinM RTVal
lookupStore i = IntMap.findWithDefault (error $ "missing location: " ++ show i) i <$> gets storeMap

evalVal :: Val -> GrinM RTVal
evalVal = \case
  v@Lit{}     -> pure $ Set.singleton v
  Var n       -> lookupEnv n
{-
  ConstTagNode t a -> ConstTagNode t $ map (evalVal env) a
  VarTagNode n a -> case lookupEnv n env of
                  Var n     -> VarTagNode n $ map (evalVal env) a
                  ValTag t  -> ConstTagNode t $ map (evalVal env) a
                  x -> error $ "evalVal - invalid VarTagNode tag: " ++ show x
-}
  v@ValTag{}  -> pure $ Set.singleton v
  v@Unit      -> pure $ Set.singleton v
  v@Loc{}     -> pure $ Set.singleton v
  x -> fail $ "evalVal: " ++ show x


evalSimpleExp :: SimpleExp -> GrinM RTVal
evalSimpleExp = \case
{-
  SApp n a -> do
              let args = map (evalVal env) a
                  go a [] [] = a
                  go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                  go _ x y = error $ "invalid pattern for function: " ++ show (n,x,y)
              case n of
                _ -> do
                  Def _ vars body <- reader $ Map.findWithDefault (error $ "unknown function: " ++ n) n
                  evalExp (go env vars args) body
-}
  SReturn v -> evalVal v

  SStore v -> do
              let l = 0 -- TODO: make it constant for each store AST operation
              v' <- evalVal v
              modify' (\computer@Computer{..} -> computer {storeMap = IntMap.insertWith mappend l v' storeMap})
              pure $ Set.singleton $ Loc l

  SFetch n -> lookupEnv n >>= \vals -> mconcat <$> mapM fetch (Set.toList vals) where
                fetch = \case
                  Loc l -> lookupStore l
                  x -> fail $ "evalSimpleExp - Fetch expected location, got: " ++ show x

  SUpdate n v -> do
              v' <- evalVal v
              let update = \case
                    Loc l -> IntMap.member l <$> gets storeMap >>= \case
                              False -> fail $ "evalSimpleExp - Update unknown location: " ++ show l
                              True  -> modify' (\computer@Computer{..} -> computer {storeMap = IntMap.insertWith mappend l v' storeMap})
                    x -> fail $ "evalSimpleExp - Update expected location, got: " ++ show x
              lookupEnv n >>= \vals -> mapM_ update (Set.toList vals) >> pure (Set.singleton Unit)

  SBlock a -> evalExp a

  x -> fail $ "evalSimpleExp: " ++ show x

evalExp :: Exp -> GrinM RTVal
evalExp = \case
  EBind op pat exp -> evalSimpleExp op >>= \v -> bindPat v pat >> evalExp exp
{-
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
-}
  exp -> evalSimpleExp exp

reduceFun :: [Def] -> Name -> (RTVal, Computer)
reduceFun l n = runState (runReaderT (evalExp e) m) emptyComputer where
  m = Map.fromList [(n,d) | d@(Def n _ _) <- l]
  e = case Map.lookup n m of
        Nothing -> error $ "missing function: " ++ n
        Just (Def _ [] a) -> a
        _ -> error $ "function " ++ n ++ " has arguments"

