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
import LLVM.AST.Type hiding (Type, void)
import qualified LLVM.AST.Type as LLVM

import Reducer.LLVM.Base
import Grin
import TypeEnv
import Pretty ()

typeGenSimpleType :: SimpleType -> LLVM.Type
typeGenSimpleType = \case
  T_Int64   -> i64
  T_Word64  -> i64
  T_Float   -> float
  T_Bool    -> i1
  T_Unit    -> LLVM.void
  T_Location _  -> ptr tagLLVMType

tagLLVMType :: LLVM.Type
tagLLVMType = i64

-- Tagged union

data TaggedUnion
  = TaggedUnion
  { tuType      :: LLVM.Type
  , tuSingleton :: Bool -- singleton is not tagged
  }

taggedUnion :: Type -> TaggedUnion
taggedUnion = undefined

{-
  simple type, node, location, tagged union
  possible conversions
    OK - simple type + node
          simple type --> node: val + build function
          simple type <-- node: lpat + project function

    NO - simple type + location
    NO - simple type + tagged union

    OK - node + location
          location --> node: val + build function
          location <-- node: lpat + project function
    OK - node + tagged union
          node --> tagged union: store, app
          node <-- tagged union: case, bind

    OK - location + tagged union

  ---------------
    NEW approach: everything is tagged union

    compilation:
      if type sets does not match then convert them

      bind    - id or prj tag index union :: simple
      case    - id or prj tag index union :: simple

      app     - id or extend ; change
      return  - id or build new ; no change
      store   - id or build new or extend ; change
      fetch   - mapping A -> mapping B ; case on loc ; change
      update  - mapping A -> mapping B ; case on loc ; change

    tagged union projection encoding:
      tagged union = TagIndex + [SimpleType]
      Map Tag (Vector Int, Vector Int) -- node order -> union, union -> node order

    tagged union construction:
      - allocate empty struct
      - fill with content from another union

    TODO:
      - union construction
      - union mapping
      - union conversion

    implement fetch

-}

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

{-
  slim = fat
  node layout = {tag + data con1 + ... + data conN}
  prj function:
    for each NodeSet have an index function/map: tag -> Int
    NodeSet -> (Type, Map Int (Value, Type))
    OR
    NodeSet -> (Type, Map Int Value) ; fat node type / conDataN type
-}

-- HINT: does hash consing
typeGenValue :: Type -> CG LLVM.Type
typeGenValue (T_SimpleType sTy) = pure $ typeGenSimpleType sTy
typeGenValue value@(T_NodeSet ns) = gets _envLLVMTypeMap >>= \tm -> case Map.lookup ns tm of
  Just t  -> pure t
  -- single node with single items e.g. {CInt[{T_Int64}]}
  _ | [(tag, items)] <- Map.toList ns -> do
        let itemTypes = [typeGenSimpleType i | i <- V.toList items]
        pure $ StructureType { isPacked = False, elementTypes = tagLLVMType : itemTypes }
  _ -> error $ printf "unsupported type: %s" (show $ pretty value)

getVarType :: Grin.Name -> CG LLVM.Type
getVarType name = do
  TypeEnv{..} <- gets _envTypeEnv
  case Map.lookup name _variable of
    Nothing -> error ("unknown variable " ++ name)
    Just value -> typeGenValue value

getFunctionType :: Grin.Name -> CG (LLVM.Type, [LLVM.Type])
getFunctionType name = do
  TypeEnv{..} <- gets _envTypeEnv
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
