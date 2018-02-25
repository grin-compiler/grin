{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Reducer.LLVM.TypeGen where

import Text.Printf

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
import Pretty

typeGenSimpleType :: SimpleType -> LLVM.Type
typeGenSimpleType = \case
  T_Int64   -> i64
  T_Word64  -> i64
  T_Float   -> float
  T_Bool    -> i1
  T_Unit    -> LLVM.void
  T_Location _  -> locationLLVMType

locationCGType :: CGType
locationCGType = toCGType $ T_SimpleType $ T_Location []

tagCGType :: CGType
tagCGType = toCGType $ T_SimpleType $ T_Int64

unitCGType :: CGType
unitCGType = toCGType $ T_SimpleType $ T_Unit

voidLLVMType :: LLVM.Type
voidLLVMType = LLVM.void

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
  allocIndex sTy = TUIndex <$> getStructIndex t <*> getArrayIndex t <*> pure t where t = typeGenSimpleType sTy

  (tuMapping, tub) = runState (mapM mapNode ns) emptyTUBuild

  tuLLVMType TUBuild{..} = StructureType
              { isPacked = True
              , elementTypes = tagLLVMType :
                               [ ArrayType (fromIntegral $ Map.findWithDefault undefined ty tubArraySizeMap) ty
                               | (ty, _idx) <- List.sortBy (\(_,a) (_,b) -> compare a b) $ Map.toList tubStructIndexMap
                               ]
              }

copyTaggedUnion :: Operand -> TaggedUnion -> TaggedUnion -> CG Operand
copyTaggedUnion srcVal srcTU dstTU | srcTU == dstTU = pure srcVal
copyTaggedUnion srcVal srcTU dstTU = do
  let -- calculate mapping
      mapping :: [(TUIndex, TUIndex)] -- src dst
      mapping = concat . map V.toList . Map.elems $ Map.intersectionWith V.zip (tuMapping srcTU) (tuMapping dstTU)
      validatedMapping = fst $ foldl validate mempty mapping
      validate (l,m) x@(src, dst) = case Map.lookup dst m of
        Nothing -> ((x:l), Map.insert dst src m)
        Just prevSrc | prevSrc == src && tuItemLLVMType src == tuItemLLVMType dst -> (l,m)
                     | otherwise      -> error $ printf "invalid tagged union mapping: %s" (show mapping)
      -- set node items
      build agg (itemType, srcIndex, dstIndex) = do
        item <- codeGenLocalVar "item" itemType $ AST.ExtractValue
          { aggregate = srcVal
          , indices'  = srcIndex
          , metadata  = []
          }
        codeGenLocalVar "node" dstTULLVMType $ AST.InsertValue
          { aggregate = agg
          , element   = item
          , indices'  = dstIndex
          , metadata  = []
          }
      tagIndex = [0]
      dstTULLVMType = tuLLVMType dstTU
      agg0 = undef dstTULLVMType
  foldM build agg0 $ (tagLLVMType, tagIndex,tagIndex) :
    [ ( tuItemLLVMType src
      , [1 + tuStructIndex src, tuArrayIndex src]
      , [1 + tuStructIndex dst, tuArrayIndex dst]
      )
    | (src,dst) <- validatedMapping
    ]

codeGenExtractTag :: Operand -> CG Operand
codeGenExtractTag tuVal = do
  codeGenLocalVar "tag" tagLLVMType $ AST.ExtractValue
    { aggregate = tuVal
    , indices'  = [0] -- tag index
    , metadata  = []
    }

codeGenBitCast :: String -> Operand -> LLVM.Type -> CG Operand
codeGenBitCast name value dstType = do
  codeGenLocalVar name dstType $ AST.BitCast
    { operand0  = value
    , type'     = dstType
    , metadata  = []
    }

{-
    NEW approach: everything is tagged union

    compilation:
      if type sets does not match then convert them
-}

codeGenValueConversion :: CGType -> Operand -> CGType -> CG Operand
codeGenValueConversion srcCGType srcOp dstCGType = case srcCGType of
  CG_SimpleType{} | srcCGType == dstCGType          -> pure srcOp
  _ | isLocation srcCGType && isLocation dstCGType  -> pure srcOp
  _ -> copyTaggedUnion srcOp (cgTaggedUnion srcCGType) (cgTaggedUnion dstCGType)
  where isLocation = \case
          CG_SimpleType{cgType = T_SimpleType (T_Location{})} -> True
          _ -> False

commonCGType :: [CGType] -> CGType
commonCGType (ty@CG_SimpleType{} : tys) | all (ty==) tys = ty
commonCGType tys | all isNodeSet tys = toCGType $ T_NodeSet $ mconcat [ns | CG_NodeSet _ (T_NodeSet ns) _ <- tys] where
  isNodeSet = \case
    CG_NodeSet{} -> True
    _ -> False
commonCGType tys = error $ printf "no common type for %s" (show $ pretty $ map cgType tys)

toCGType :: Type -> CGType
toCGType t = case t of
  T_SimpleType sTy  -> CG_SimpleType (typeGenSimpleType sTy) t
  T_NodeSet ns      -> CG_NodeSet (tuLLVMType tu) t tu where tu = taggedUnion ns

getVarType :: Grin.Name -> CG CGType
getVarType name = do
  TypeEnv{..} <- gets _envTypeEnv
  case Map.lookup name _variable of
    Nothing -> error ("unknown variable " ++ name)
    Just ty -> pure $ toCGType ty

getFunctionType :: Grin.Name -> CG (CGType, [CGType])
getFunctionType name = do
  TypeEnv{..} <- gets _envTypeEnv
  case Map.lookup name _function of
    Nothing -> error $ printf "unknown function %s" name
    Just (retValue, argValues) -> do
      retType <- pure $ toCGType retValue
      argTypes <- pure $ map toCGType $ V.toList argValues
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
