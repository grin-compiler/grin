{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Reducer.LLVM.TypeGen where

import Debug.Trace
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen (pretty)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.State
import Lens.Micro.Platform

import LLVM.AST.Constant as C hiding (Add, ICmp)
import LLVM.AST.Type as LLVM

import Reducer.LLVM.Base
import AbstractInterpretation.HPTResultNew
import AbstractInterpretation.PrettyHPT ()
import Grin
import Pretty ()

typeGenSimpleType :: SimpleType -> Type
typeGenSimpleType = \case
  T_Int64   -> i64
  T_Word64  -> i64
  T_Float   -> float
  T_Bool    -> i1
  T_Unit    -> LLVM.void

{-
      modify' (\env@Env{..} -> env {envDefinitions = def : envDefinitions})

      TypeDefinition (UnName 0) (
         Just $ StructureType False [
           i32,
           ptr (NamedTypeReference (UnName 1)),
           ptr (NamedTypeReference (UnName 0))
          ]),
      TypeDefinition (UnName 1) Nothing,
-}

-- HINT: does hash consing
typeGenValue :: Value -> CG Type
typeGenValue value = gets _envTypeMap >>= \tm -> case Map.lookup value tm of
  Just t  -> pure t
  -- single simple type e.g. T_Int64
  _ | [SimpleType sTy] <- Set.elems (value^.simpleTypeAndLocationSet)
    , Map.null (value^.nodeSet^.nodeTagMap) -> pure $ typeGenSimpleType sTy
  -- single location type e.g. {1}
  _ | [Location loc] <- Set.elems (value^.simpleTypeAndLocationSet)
    , Map.null (value^.nodeSet^.nodeTagMap) -> do
        nodeSet <- use $ envHPTResult.memory.ix loc
        t <- typeGenValue $ Value mempty nodeSet
        pure $ ptr t
  -- single node with single items e.g. {CInt[{T_Int64}]}
  _ | Set.null (value^.simpleTypeAndLocationSet)
    , [(tag, items)] <- Map.toList (value^.nodeSet^.nodeTagMap)
    , all (\s -> 1 == Set.size s) items -> do
        itemTypes <- sequence [typeGenValue (Value i mempty) | i <- V.toList items]
        let tagType = i64 -- TODO
        pure $ StructureType { isPacked = False, elementTypes = tagType : itemTypes }
  -- multiple nodes with single items e.g. {CInt[{T_Int64}], Fadd[{1},{2}]}
  _ | Set.null (value^.simpleTypeAndLocationSet)
    , not $ Map.null (value^.nodeSet^.nodeTagMap) -> do
        let (tagList, itemsList) = unzip $ Map.toList (value^.nodeSet^.nodeTagMap)
            conDataStruct items = do
              itemTypes <- sequence [typeGenValue (Value i mempty) | i <- V.toList items]
              pure $ StructureType { isPacked = False, elementTypes = itemTypes }
        conDataStructs <- mapM conDataStruct itemsList
        let tagType = i64 -- TODO
        pure $ StructureType { isPacked = False, elementTypes = tagType : conDataStructs }
  -- multiple location type e.g. {1,2,3}
  _ | not $ Set.null (value^.simpleTypeAndLocationSet)
    , Map.null (value^.nodeSet^.nodeTagMap) -> do
        -- HACK !!!
        nodeSet <-  mconcat <$> sequence [use (envHPTResult.memory.ix loc) | Location loc <- Set.elems (value^.simpleTypeAndLocationSet)]
        t <- typeGenValue $ Value mempty nodeSet
        pure $ ptr t
  _ -> fail $ printf "unsupported type: %s" (show $ pretty value)

getVarType :: Grin.Name -> CG Type
getVarType name = do
  HPTResult{..} <- gets _envHPTResult
  case Map.lookup name _register of
    Nothing -> fail ("unknown variable " ++ name)
    Just value -> typeGenValue value

getFunctionType :: Grin.Name -> CG (Type, [Type])
getFunctionType name = do
  HPTResult{..} <- gets _envHPTResult
  case Map.lookup name _function of
    Nothing -> error $ printf "unknown function %s" name
    Just (retValue, argValues) -> do
      retType <- typeGenValue retValue
      argTypes <- mapM typeGenValue $ V.toList argValues
      pure (retType, argTypes)

getTagId :: Tag -> Constant
getTagId tag = case Map.lookup tag tagMap of
  Nothing -> trace ("unknown tag " ++ show (pretty tag)) $ Int 64 0
  Just (ty, c) -> c
 where
  -- TODO: create Tag map ; get as parameter ; store in reader environment
  {-
    question: how to calculate from grin or hpt result?
  -}
  tagMap :: Map Tag (Type, Constant)
  tagMap = Map.fromList []
