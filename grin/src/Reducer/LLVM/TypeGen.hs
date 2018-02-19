{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Reducer.LLVM.TypeGen where

import Debug.Trace
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen (pretty)

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as List

import Control.Monad.State
import Lens.Micro.Platform

import LLVM.AST as AST hiding (Type, void)
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
{-
  HINT: tagged union LLVM representation

    struct {
      Int64[N1];
      Word64[N2];
      ...
    }
-}
data TUIndex
  = TUIndex
  { tuStructIndex :: Word32
  , tuArrayIndex  :: Word32
  }
  deriving (Eq, Ord, Show)

data TaggedUnion
  = TaggedUnion
  { tuLLVMType  :: LLVM.Type -- struct of arrays of SimpleType with size
  , tuMapping   :: Map Tag (Vector TUIndex)
  }
  deriving (Eq, Ord, Show)


data TUBuild
  = TUBuild
  { tubStructIndexMap :: Map LLVM.Type Word32
  , tubArraySizeMap   :: Map LLVM.Type Word32
  , tubArrayPosMap    :: Map LLVM.Type Word32
  }

emptyTUBuild = TUBuild mempty mempty mempty

type TU = State TUBuild

taggedUnion :: NodeSet -> TaggedUnion
taggedUnion ns = TaggedUnion (tuLLVMType tub) tuMapping where

  mapNode :: Vector SimpleType -> TU (Vector TUIndex)
  mapNode v = do
    nodeMapping <- mapM allocIndex v
    modify $ \tub@TUBuild{..} -> tub {tubArraySizeMap = Map.unionWith max tubArrayPosMap tubArraySizeMap, tubArrayPosMap = mempty}
    pure nodeMapping

  getStructIndex :: LLVM.Type -> TU Word32
  getStructIndex ty = state $ \tub@TUBuild{..} ->
    let i = Map.findWithDefault (fromIntegral $ Map.size tubStructIndexMap) ty tubStructIndexMap
    in (i, tub {tubStructIndexMap = Map.insert ty i tubStructIndexMap})

  getArrayIndex :: LLVM.Type -> TU Word32
  getArrayIndex ty = state $ \tub@TUBuild{..} ->
    let i = Map.findWithDefault 0 ty tubArrayPosMap
    in (i, tub {tubArrayPosMap = Map.insert ty (succ i) tubArrayPosMap})

  allocIndex :: SimpleType -> TU TUIndex
  allocIndex sTy = TUIndex <$> getStructIndex t <*> getArrayIndex t where t = typeGenSimpleType sTy

  (tuMapping, tub) = runState (mapM mapNode ns) emptyTUBuild

  tuLLVMType TUBuild{..} = StructureType
              { isPacked = True
              , elementTypes = tagLLVMType :
                               [ ArrayType (fromIntegral $ tubArraySizeMap Map.! ty) ty
                               | (ty, _idx) <- List.sortBy (\(_,a) (_,b) -> compare a b) $ Map.toList tubStructIndexMap
                               ]
              }

copyTaggedUnion :: Operand -> TaggedUnion -> TaggedUnion -> CG Operand
copyTaggedUnion srcVal srcTU dstTU = do
  let -- calculate mapping
      mapping :: [(TUIndex, TUIndex)] -- src dst
      mapping = concat . map V.toList . Map.elems $ Map.intersectionWith V.zip (tuMapping srcTU) (tuMapping dstTU)
      validatedMapping = fst $ foldl validate mempty mapping
      validate (l,m) x@(src, dst) = case Map.lookup dst m of
        Nothing -> ((x:l), Map.insert dst src m)
        Just prevSrc | prevSrc == src -> (l,m)
                     | otherwise      -> error $ printf "invalid tagged union mapping: %s" (show mapping)
      -- set node items
      build mAgg (srcIndex, dstIndex) = do
        agg <- getOperand mAgg
        item <- getOperand $ I $ AST.ExtractValue
          { aggregate = srcVal
          , indices'  = srcIndex
          , metadata  = []
          }
        pure $ I $ AST.InsertValue
          { aggregate = agg
          , element   = item
          , indices'  = dstIndex
          , metadata  = []
          }
      tagIndex = [0]
      agg0 = O (undef (tuLLVMType dstTU)) undefined -- FIXME
  agg <- foldM build agg0 $ (tagIndex,tagIndex) :
    [ ( [1 + tuStructIndex src, tuArrayIndex src]
      , [1 + tuStructIndex dst, tuArrayIndex dst]
      )
    | (src,dst) <- validatedMapping
    ]
  getOperand agg

{-
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
      done - allocate empty struct
      done - fill with content from another union

    TODO:
      done - union construction
      done - union mapping
      done - union conversion
              - copy    (DST array size > SRC array size) -> copy SRC arrays to DST item by item ; llvm does not have instruction for this
                        (DST array size = SRC array size) -> bypass
              - shrink  (DST array size < SRC array size) -> copy SRC arrays to DST item by item

    done - implement fetch

-}

-- HINT: does hash consing
typeGenValue :: Type -> CG LLVM.Type
typeGenValue (T_SimpleType sTy) = pure $ typeGenSimpleType sTy
typeGenValue value@(T_NodeSet ns) = gets _envLLVMTypeMap >>= \tm -> case Map.lookup ns tm of
  Just t  -> pure t
  _ -> do
        let tu = taggedUnion ns
        pure $ tuLLVMType tu
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

getTagId :: Tag -> CG Constant
getTagId tag = do
  tagMap <- use envTagMap
  case Map.lookup tag tagMap of
    Just c  -> pure c
    Nothing -> do
      let c = Int 64 $ fromIntegral $ Map.size tagMap
      envTagMap %= (Map.insert tag c)
      pure c
