{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Reducer.LLVM.InferType where

import Debug.Trace
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen (pretty)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.State
import Lens.Micro.Platform

import Reducer.LLVM.Base
import Grin
import TypeEnv
import Pretty ()

-- allows simple type singletons or locations
validateNodeItem :: Type -> CG ()
validateNodeItem ts@T_NodeSet{} = fail $ printf "illegal node item type %s" (show $ pretty ts)
validateNodeItem _ = pure ()

nodeType :: Tag -> [Type] -> CG Type
nodeType tag items = do
  mapM_ validateNodeItem items
  pure $ T_NodeSet $ Map.singleton tag $ V.fromList $ map _simpleType items

typeOfLit :: Lit -> Type
typeOfLit lit = T_SimpleType $ case lit of
  LInt64{}  -> T_Int64
  LWord64{} -> T_Word64
  LFloat{}  -> T_Float
  LBool{}   -> T_Bool

typeOfVal :: Val -> CG Type
typeOfVal val = do
  case val of
    ConstTagNode tag args -> mapM typeOfVal args >>= nodeType tag
    {-
    VarTagNode    Name [SimpleVal] -- complete node (variable tag)
    ValTag        Tag
    -}
    Unit      -> pure $ T_SimpleType T_Unit
    Lit lit   -> pure $ typeOfLit lit
    Var name  -> use (envTypeEnv.variable.at name) >>= \case
                  Nothing -> error $ printf "unknown variable %s" name
                  Just ty -> pure ty
    _ -> error $ printf "unsupported val" (show $ pretty val)
