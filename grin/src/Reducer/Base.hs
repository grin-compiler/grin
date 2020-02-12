{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}
module Reducer.Base where

import Data.IntMap.Strict (IntMap)
import Data.Map (Map)
import Grin.Grin
import Grin.Pretty
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Map as Map
import qualified Data.IntMap.Strict as IntMap


-- models cpu registers
type Env = Map Name RTVal

type SimpleRTVal = RTVal
data RTVal
  = RT_ConstTagNode Tag  [SimpleRTVal]
  | RT_VarTagNode   Name [SimpleRTVal]
  | RT_ValTag       Tag
  | RT_Unit
  | RT_Lit          Lit
  | RT_Var          Name
  | RT_Loc          Int
  | RT_Undefined
  deriving (Show, Eq, Ord)

instance Pretty RTVal where
  pretty = \case
    RT_ConstTagNode tag args -> parens $ hsep (pretty tag : map pretty args)
    RT_VarTagNode name args  -> parens $ hsep (pretty name : map pretty args)
    RT_ValTag tag -> pretty tag
    RT_Unit       -> parens empty
    RT_Lit lit    -> pretty lit
    RT_Var name   -> pretty name
    RT_Loc a      -> keyword "loc" <+> int a
    RT_Undefined  -> keyword "undefined"

data Statistics
  = Statistics
  { storeFetched :: !(IntMap Int)
  , storeUpdated :: !(IntMap Int)
  }

emptyStatistics = Statistics mempty mempty

instance Pretty Statistics where
  pretty (Statistics f u) =
    vsep
      [ text "Fetched:"
      , indent 4 $ prettyKeyValue $ IntMap.toList $ IntMap.filter (>0) f
      , text "Updated:"
      , indent 4 $ prettyKeyValue $ IntMap.toList $ IntMap.filter (>0) u
      ]

keyword :: String -> Doc
keyword = yellow . text

selectNodeItem :: Maybe Int -> RTVal -> RTVal
selectNodeItem Nothing val = val
selectNodeItem (Just 0) (RT_ConstTagNode tag args) = RT_ValTag tag
selectNodeItem (Just i) (RT_ConstTagNode tag args) = args !! (i - 1)

bindPatMany :: Env -> [RTVal] -> [LPat] -> Env
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env RT_Undefined lpat) [] lpats
bindPatMany _ vals lpats = error $ "bindPatMany - pattern mismatch: " ++ show (vals, lpats)

bindPat :: Env -> RTVal -> LPat -> Env
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

lookupEnv :: Name -> Env -> RTVal
lookupEnv n env = Map.findWithDefault (error $ "missing variable: " ++ unpackName n) n env

evalVal :: Env -> Val -> RTVal
evalVal env = \case
  Lit lit     -> RT_Lit lit
  Var n       -> lookupEnv n env
  ConstTagNode t a -> RT_ConstTagNode t $ map (evalVal env) a
  VarTagNode n a -> case lookupEnv n env of
                  RT_Var n     -> RT_VarTagNode n $ map (evalVal env) a
                  RT_ValTag t  -> RT_ConstTagNode t $ map (evalVal env) a
                  x -> error $ "evalVal - invalid VarTagNode tag: " ++ show x
  ValTag tag  -> RT_ValTag tag
  Unit        -> RT_Unit
  Undefined t -> RT_Undefined
  x -> error $ "evalVal: " ++ show x
