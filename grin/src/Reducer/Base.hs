{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}
module Reducer.Base where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint.ANSI.Leijen

import Grin.Grin
import Grin.Pretty

-- models cpu registers
type Env v = Map Name (RTVal v)

type SimpleRTVal v = RTVal v
data RTVal v
  = RT_ConstTagNode Tag  [SimpleRTVal v]
  | RT_VarTagNode   Name [SimpleRTVal v]
  | RT_ValTag       Tag
  | RT_Unit
  | RT_Lit          v
  | RT_Var          Name
  | RT_Loc          Int
  | RT_Undefined
  deriving (Show, Eq, Ord)

instance Pretty v => Pretty (RTVal v) where
  pretty = \case
    RT_ConstTagNode tag args -> parens $ hsep (pretty tag : map pretty args)
    RT_VarTagNode name args  -> parens $ hsep (pretty name : map pretty args)
    RT_ValTag tag -> pretty tag
    RT_Unit       -> parens empty
    RT_Lit lit    -> pretty lit
    RT_Var name   -> pretty name
    RT_Loc a      -> keyword "loc" <+> int a
    RT_Undefined  -> keyword "undefined"

keyword :: String -> Doc
keyword = yellow . text

selectNodeItem :: Maybe Int -> RTVal v -> RTVal v
selectNodeItem Nothing val = val
selectNodeItem (Just 0) (RT_ConstTagNode tag args) = RT_ValTag tag
selectNodeItem (Just i) (RT_ConstTagNode tag args) = args !! (i - 1)

bindPatMany :: (Show v) => Env v -> [RTVal v] -> [LPat] -> Env v
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env RT_Undefined lpat) [] lpats
bindPatMany _ vals lpats = error $ "bindPatMany - pattern mismatch: " ++ show (vals, lpats)

bindPat :: (Show v) => Env v -> RTVal v -> LPat -> Env v
bindPat env !val lpat = case lpat of
  Var n -> case val of
              RT_Unit       -> Map.insert n val env
              RT_Lit{}      -> Map.insert n val env
              RT_Loc{}      -> Map.insert n val env
              RT_Undefined  -> Map.insert n val env
              RT_ConstTagNode{} -> Map.insert n val env
              _ -> error $ "bindPat - illegal value: " ++ show val
  ConstTagNode ptag pargs -> case val of
                  RT_ConstTagNode vtag vargs | ptag == vtag -> bindPatMany env vargs pargs
                  _ -> error $ "bindPat - illegal value for ConstTagNode: " ++ show val ++ " vs " ++ show (ConstTagNode ptag pargs)
  VarTagNode varname pargs -> case val of
                  RT_ConstTagNode vtag vargs -> bindPatMany (Map.insert varname (RT_ValTag vtag) env) vargs pargs
                  _ -> error $ "bindPat - illegal value for ConstTagNode: " ++ show val
  Unit -> env
  _ -> error $ "bindPat - pattern mismatch" ++ show (val,lpat)

lookupEnv :: Name -> Env v -> RTVal v
lookupEnv n env = Map.findWithDefault (error $ "missing variable: " ++ unpackName n) n env

evalVal :: (Show v) => (Lit -> v) -> Env v -> Val -> RTVal v
evalVal evalLit env = \case
  Lit lit     -> RT_Lit $ evalLit lit
  Var n       -> lookupEnv n env
  ConstTagNode t a -> RT_ConstTagNode t $ map (evalVal evalLit env) a
  VarTagNode n a -> case lookupEnv n env of
                  RT_Var n     -> RT_VarTagNode n $ map (evalVal evalLit env) a
                  RT_ValTag t  -> RT_ConstTagNode t $ map (evalVal evalLit env) a
                  x -> error $ "evalVal - invalid VarTagNode tag: " ++ show x
  ValTag tag  -> RT_ValTag tag
  Unit        -> RT_Unit
  Undefined t -> RT_Undefined
  x -> error $ "evalVal: " ++ show x
