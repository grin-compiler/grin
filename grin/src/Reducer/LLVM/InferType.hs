{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Reducer.LLVM.InferType where

import Text.Printf

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.State
import Lens.Micro.Platform

import Reducer.LLVM.Base
import Grin
import TypeEnv hiding (typeOfVal)
import Pretty

-- TODO: replace this module with a more generic one that could be used by other components also

-- allows simple type singletons or locations
validateNodeItem :: Type -> CG ()
validateNodeItem ts@T_NodeSet{} = error $ printf "illegal node item type %s" (show $ pretty ts)
validateNodeItem _ = pure ()

nodeType :: Tag -> [Type] -> CG Type
nodeType tag items = do
  mapM_ validateNodeItem items
  pure $ T_NodeSet $ Map.singleton tag $ V.fromList $ map _simpleType items

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
    _ -> error $ printf "unsupported val %s" (show $ pretty val)
