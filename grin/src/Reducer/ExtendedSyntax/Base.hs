{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}
module Reducer.ExtendedSyntax.Base where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (fold)

import Text.PrettyPrint.ANSI.Leijen

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty

-- models cpu registers
type Env = Map Name RTVal

type SimpleRTVal = RTVal
data RTVal
  = RT_ConstTagNode Tag  [SimpleRTVal]
  | RT_Unit
  | RT_Lit          Lit
  | RT_Var          Name
  | RT_Loc          Int
  | RT_Undefined
  deriving (Show, Eq, Ord)


instance Pretty RTVal where
  pretty = \case
    RT_ConstTagNode tag args -> parens $ hsep (pretty tag : map pretty args)
    RT_Unit       -> parens empty
    RT_Lit lit    -> pretty lit
    RT_Var name   -> pretty name
    RT_Loc a      -> keyword "loc" <+> int a
    RT_Undefined  -> keyword "undefined"

keyword :: String -> Doc
keyword = yellow . text

selectNodeItem :: Maybe Int -> RTVal -> RTVal
selectNodeItem Nothing val = val
selectNodeItem (Just i) (RT_ConstTagNode tag args) = args !! (i - 1)

bindPat :: Env -> RTVal -> BPat -> Env
bindPat env !val bPat = case bPat of
  VarPat var -> Map.insert var val env
  p@(AsPat tag args var) -> case val of
    RT_ConstTagNode vtag vargs
      | tag == vtag
      , env' <- Map.insert var val env
      , newVars <- fold $ zipWith Map.singleton args vargs
      -> env' <> newVars
    _ -> error $ "bindPat - illegal value for ConstTagNode: " ++ show val ++ " vs " ++ show (PP p)

lookupEnv :: Name -> Env -> RTVal
lookupEnv n env = Map.findWithDefault (error $ "missing variable: " ++ unpackName n) n env

evalVal :: Env -> Val -> RTVal
evalVal env = \case
  Lit lit          -> RT_Lit lit
  Var n            -> lookupEnv n env
  ConstTagNode t a -> RT_ConstTagNode t $ map (`lookupEnv` env) a
  Unit             -> RT_Unit
  Undefined t      -> RT_Undefined
  x -> error $ "evalVal: " ++ show x
