{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, OverloadedStrings #-}
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

import Grin.Grin (Tag, Exp, Name, packName)
import Grin.Pretty
import AbstractInterpretation.PrettyHPT

import AbstractInterpretation.HPTResult
import qualified Grin.TypeEnv as TypeEnv

import AbstractInterpretation.IR (HasDataFlowInfo(..))
import AbstractInterpretation.Reduce (AbstractInterpretationResult(..))
import qualified AbstractInterpretation.HeapPointsTo as HPT
import qualified AbstractInterpretation.HPTResult as HPT
import qualified AbstractInterpretation.Reduce as R

import Data.Bifunctor

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

  convertSimpleTypeSet :: Name -> NodeSet -> [SimpleType] -> Either String TypeEnv.SimpleType
  convertSimpleTypeSet _ _ [] = pure TypeEnv.T_Dead
  convertSimpleTypeSet _ _ [sTy] = convertSimpleType sTy
  convertSimpleTypeSet n ns tys
    | all isLocation tys
    -- skipping unspecified locations
    , locs <- [l | T_Location l <- tys]
    = if null locs then pure $ TypeEnv.T_UnspecifiedLocation
                   else pure $ TypeEnv.T_Location locs
    | otherwise = throwError $ printf "%s has illegal node item type %s in %s" n (show . pretty $ Set.fromList tys) (show $ pretty ns)

  convertNodeSet :: Name -> NodeSet -> Either String (Map Tag (Vector TypeEnv.SimpleType))
  convertNodeSet n a@(NodeSet ns) = mapM (mapM (convertSimpleTypeSet n a . Set.toList)) ns

  convertLocation :: Int -> NodeSet -> Either String (Map Tag (Vector TypeEnv.SimpleType))
  convertLocation loc = convertNodeSet $ "Loc " <> (packName . show $ loc)

  convertTypeSet :: Name -> TypeSet -> Either String TypeEnv.Type
  convertTypeSet n ts = do
    let ns = ts^.nodeSet
        st = ts^.simpleType
    case (Set.size st, Map.size $ ns^.nodeTagMap) of
      (stCount,nsCount) | stCount == 0 && nsCount > 0 -> TypeEnv.T_NodeSet <$> convertNodeSet n ns
      (stCount,nsCount) | stCount > 0 && nsCount == 0 -> TypeEnv.T_SimpleType <$> convertSimpleTypeSet n ns (Set.toList st)
      (0,0)                                           -> pure TypeEnv.dead_t
      _ -> throwError $ printf "%s has illegal type of %s" n (show . pretty $ ts)

  convertFunction :: Name -> (TypeSet, Vector TypeSet) -> Either String (TypeEnv.Type, Vector TypeEnv.Type)
  convertFunction name (ret, args) = (,) <$> convertTypeSet name ret <*> mapM (convertTypeSet name) args

  typeEnv :: Either String TypeEnv.TypeEnv
  typeEnv = TypeEnv.TypeEnv <$>
    (V.imapM convertLocation (_memory hptResult)) <*>
    (Map.traverseWithKey convertTypeSet $ (_register hptResult)) <*>
    (Map.traverseWithKey convertFunction $ (_function hptResult))


-- TODO: Add mode as a parameter?
inferTypeEnv :: Exp -> TypeEnv.TypeEnv
inferTypeEnv exp = either error id $ typeEnvFromHPTResult =<< result where
  hptProgram = HPT.codeGen exp
  computer = (_crComp . R.evalDataFlowInfo . getDataFlowInfo) <$> hptProgram
  result = HPT.toHPTResult <$>  hptProgram <*> computer
