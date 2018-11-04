{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module Grin.TypeCheck where

import Text.Printf

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.Except
import Lens.Micro.Platform

import Grin.Grin (Tag, Exp)
import Grin.Pretty
import AbstractInterpretation.PrettyHPT

import AbstractInterpretation.HPTResult
import qualified Grin.TypeEnv as TypeEnv

import AbstractInterpretation.IR (HasDataFlowInfo(..))
import qualified AbstractInterpretation.HeapPointsTo as HPT
import qualified AbstractInterpretation.HPTResult as HPT
import qualified AbstractInterpretation.Reduce as R

{-
  validate HPT result
         - fixed node tag arity
    done - monomorph nodes ; each node component is
            - singleton simple type
            - one or more locations
            - unspecified location
            - dead type (empty simple typeset)
-}
typeEnvFromHPTResult :: HPTResult -> Either String TypeEnv.TypeEnv
typeEnvFromHPTResult hptResult = typeEnv where
  convertSimpleType :: SimpleType -> Either String TypeEnv.SimpleType
  convertSimpleType = \case
    T_Int64               -> pure TypeEnv.T_Int64
    T_Word64              -> pure TypeEnv.T_Word64
    T_Float               -> pure TypeEnv.T_Float
    T_Bool                -> pure TypeEnv.T_Bool
    T_Unit                -> pure TypeEnv.T_Unit
    T_UnspecifiedLocation -> pure TypeEnv.T_UnspecifiedLocation
    T_Location l          -> pure $ TypeEnv.T_Location [l]
    l@Local{} -> throwError $ "Encountered analysis specific local value during type checking: " ++ show l

  isLocation :: SimpleType -> Bool
  isLocation = \case
    T_Location _ -> True
    T_UnspecifiedLocation -> True
    _ -> False

  convertSimpleTypeSet :: NodeSet -> [SimpleType] -> Either String TypeEnv.SimpleType
  convertSimpleTypeSet _ [] = pure TypeEnv.T_Dead
  convertSimpleTypeSet _ [sTy] = convertSimpleType sTy
  convertSimpleTypeSet ns tys
    | all isLocation tys
    -- skipping unspecified locations
    , locs <- [l | T_Location l <- tys]
    = if null locs then pure $ TypeEnv.T_UnspecifiedLocation
                   else pure $ TypeEnv.T_Location locs
    | otherwise = throwError $ printf "illegal node item type %s in %s" (show . pretty $ Set.fromList tys) (show $ pretty ns)

  convertNodeSet :: NodeSet -> Either String (Map Tag (Vector TypeEnv.SimpleType))
  convertNodeSet a@(NodeSet ns) = mapM (mapM (convertSimpleTypeSet a . Set.toList)) ns

  convertTypeSet :: TypeSet -> Either String TypeEnv.Type
  convertTypeSet ts = do
    let ns = ts^.nodeSet
        st = ts^.simpleType
    case (Set.size st, Map.size $ ns^.nodeTagMap) of
      (stCount,nsCount) | stCount == 0 && nsCount > 0 -> TypeEnv.T_NodeSet <$> convertNodeSet ns
      (stCount,nsCount) | stCount > 0 && nsCount == 0 -> TypeEnv.T_SimpleType <$> convertSimpleTypeSet ns (Set.toList st)
      (0,0)                                           -> pure TypeEnv.dead_t
      _ -> throwError $ printf "illegal type %s" (show . pretty $ ts)

  convertFunction :: (TypeSet, Vector TypeSet) -> Either String (TypeEnv.Type, Vector TypeEnv.Type)
  convertFunction (ret, args) = (,) <$> convertTypeSet ret <*> mapM convertTypeSet args

  typeEnv :: Either String TypeEnv.TypeEnv
  typeEnv = TypeEnv.TypeEnv <$>
    mapM convertNodeSet  (_memory hptResult) <*>
    mapM convertTypeSet  (_register hptResult) <*>
    mapM convertFunction (_function hptResult)

inferTypeEnv :: Exp -> TypeEnv.TypeEnv
inferTypeEnv exp = either error id $ typeEnvFromHPTResult =<< result where
  hptProgram = HPT.codeGen exp
  hptResult = (R.evalDataFlowInfo . getDataFlowInfo) <$> hptProgram
  result = HPT.toHPTResult <$>  hptProgram <*> hptResult
