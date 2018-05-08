{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module TypeCheck where

import Text.Printf
import Text.PrettyPrint.ANSI.Leijen

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V

import Lens.Micro.Platform

import Grin (Tag, Exp)
import Pretty
import AbstractInterpretation.PrettyHPT

import AbstractInterpretation.HPTResultNew
import qualified TypeEnv

import qualified AbstractInterpretation.CodeGen as HPT
import qualified AbstractInterpretation.Reduce as HPT

{-
  validate HPT result
         - fixed node tag arity
    done - monomorph nodes ; each node component is
            - singleton simple type
            - one or more location
-}
typeEnvFromHPTResult :: HPTResult -> TypeEnv.TypeEnv
typeEnvFromHPTResult hptResult = typeEnv where
  convertSimpleType :: SimpleType -> TypeEnv.SimpleType
  convertSimpleType = \case
    T_Int64   -> TypeEnv.T_Int64
    T_Word64  -> TypeEnv.T_Word64
    T_Float   -> TypeEnv.T_Float
    T_Bool    -> TypeEnv.T_Bool
    T_Unit    -> TypeEnv.T_Unit
    T_Location l -> TypeEnv.T_Location [l]

  isLocation :: SimpleType -> Bool
  isLocation = \case
    T_Location _ -> True
    _ -> False

  convertNodeItem :: [SimpleType] -> TypeEnv.SimpleType
  convertNodeItem [sTy] = convertSimpleType sTy
  convertNodeItem tys | all isLocation tys = TypeEnv.T_Location [l | T_Location l <- tys]
  convertNodeItem tys = error $ printf "illegal type %s" (show . pretty $ Set.fromList tys)

  convertNodeSet :: NodeSet -> Map Tag (Vector TypeEnv.SimpleType)
  convertNodeSet (NodeSet ns) = Map.map (V.map (convertNodeItem . Set.toList)) ns

  convertTypeSet :: TypeSet -> TypeEnv.Type
  convertTypeSet ts = let ns = ts^.nodeSet
                          st = ts^.simpleType
                      in case (Set.size st, Map.size $ ns^.nodeTagMap) of
                          (stCount,nsCount) | stCount == 0 && nsCount > 0 -> TypeEnv.T_NodeSet $ convertNodeSet ns
                          (stCount,nsCount) | stCount > 0 && nsCount == 0 -> TypeEnv.T_SimpleType $ convertNodeItem $ Set.toList st
                          _ -> error $ printf "illegal type %s" (show . pretty $ ts)

  convertFunction :: (TypeSet, Vector TypeSet) -> (TypeEnv.Type, Vector TypeEnv.Type)
  convertFunction (ret, args) = (convertTypeSet ret, V.map convertTypeSet args)

  typeEnv :: TypeEnv.TypeEnv
  typeEnv = TypeEnv.TypeEnv
    { _location = V.map convertNodeSet $ _memory hptResult
    , _variable = Map.map convertTypeSet $ _register hptResult
    , _function = Map.map convertFunction $ _function hptResult
    }

inferTypeEnv :: Exp -> TypeEnv.TypeEnv
inferTypeEnv exp = typeEnvFromHPTResult result where
  hptProgram = HPT.codeGen exp
  hptResult = HPT.evalHPT hptProgram
  result = HPT.toHPTResult hptProgram hptResult
