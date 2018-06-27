{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}
module Reducer.Base where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen

import Grin
import Pretty

-- models cpu registers
type Env = Map Name RTVal

data RTVal
  = Val Val
  | Loc Int
  | Undefined
  deriving (Show, Eq, Ord)

instance Pretty RTVal where
  pretty = \case
    Val val   -> pretty val
    Loc a     -> keyword "loc" <+> int a
    Undefined -> keyword "undefined"

keyword :: String -> Doc
keyword = yellow . text

selectNodeItem :: Maybe Int -> RTVal -> RTVal
selectNodeItem Nothing val = val
selectNodeItem (Just 0) (Val (ConstTagNode tag args)) = Val $ ValTag tag
selectNodeItem (Just i) (Val (ConstTagNode tag args)) = Val $ args !! (i - 1)

bindPatMany :: Env -> [RTVal] -> [LPat] -> Env
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env Undefined lpat) [] lpats
bindPatMany _ vals lpats = error $ "bindPatMany - pattern mismatch: " ++ show (vals, lpats)

bindPat :: Env -> RTVal -> LPat -> Env
bindPat env !val lpat = case lpat of
{-
  Var n -> case val of
              Val (ValTag{})  -> Map.insert n val env
              Val (Unit)      -> Map.insert n val env
              Val (Lit{})     -> Map.insert n val env
              Loc{}           -> Map.insert n val env
              Undefined       -> Map.insert n val env
              _ -> Map.insert n val env -- WTF????
              _ -> error $ "bindPat - illegal value: " ++ show val
  ConstTagNode ptag pargs -> case val of
                  ConstTagNode vtag vargs | ptag == vtag -> bindPatMany env vargs pargs
                  _ -> error $ "bindPat - illegal value for ConstTagNode: " ++ show val
  VarTagNode varname pargs -> case val of
                  ConstTagNode vtag vargs -> bindPatMany (Map.insert varname (ValTag vtag) env) vargs pargs
                  _ -> error $ "bindPat - illegal value for ConstTagNode: " ++ show val
  Unit -> env
-}
  _ -> error $ "bindPat - pattern mismatch" ++ show (val,lpat)

lookupEnv :: Name -> Env -> RTVal
lookupEnv n env = Map.findWithDefault (error $ "missing variable: " ++ n) n env

evalVal :: Env -> Val -> RTVal
evalVal env = \case
{-
  v@Lit{}     -> Val v
  Var n       -> lookupEnv n env
  ConstTagNode t a -> ConstTagNode t $ map (evalVal env) a
  VarTagNode n a -> case lookupEnv n env of
                  Var n     -> VarTagNode n $ map (evalVal env) a
                  ValTag t  -> ConstTagNode t $ map (evalVal env) a
                  x -> error $ "evalVal - invalid VarTagNode tag: " ++ show x
  v@ValTag{}  -> v
  v@Unit      -> v
  v@Loc{}     -> v
-}
  x -> error $ "evalVal: " ++ show x
