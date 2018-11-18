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

<<<<<<< HEAD
import AbstractInterpretation.IR (HasDataFlowInfo(..))
import qualified AbstractInterpretation.HeapPointsTo as HPT
import qualified AbstractInterpretation.HPTResult as HPT
import qualified AbstractInterpretation.Reduce as R
=======
import qualified AbstractInterpretation.CodeGenMain as HPT
import qualified AbstractInterpretation.Reduce as HPT
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70

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

  convertSimpleTypeSet :: NodeSet -> [SimpleType] -> Either String TypeEnv.SimpleType
  convertSimpleTypeSet _ [] = pure TypeEnv.T_Dead
  convertSimpleTypeSet _ [sTy] = convertSimpleType sTy
  convertSimpleTypeSet ns tys
    | all isLocation tys
<<<<<<< HEAD
    -- skipping unspecified locations
    , locs <- [l | T_Location l <- tys]
    = if null locs then pure $ TypeEnv.T_UnspecifiedLocation
                   else pure $ TypeEnv.T_Location locs
    | otherwise = throwError $ printf "illegal node item type %s in %s" (show . pretty $ Set.fromList tys) (show $ pretty ns)

  convertNodeSet :: NodeSet -> Either String (Map Tag (Vector TypeEnv.SimpleType))
  convertNodeSet a@(NodeSet ns) = mapM (mapM (convertSimpleTypeSet a . Set.toList)) ns
=======
    = pure $ TypeEnv.T_Location [l | T_Location l <- tys]
  convertNodeItem tys = throwError $ printf "illegal node item type %s" (show . pretty $ Set.fromList tys)

  checkNode :: Name -> NodeSet -> Vector TypeEnv.SimpleType -> Either String (Vector TypeEnv.SimpleType)
  checkNode n ns v
    | any (TypeEnv.T_Location [] ==) v = throwError $ printf "%s: illegal node type %s in %s" n (show . pretty $ V.toList v) (show $ pretty ns)
    | otherwise = pure v

  convertNodeSet :: (Name, NodeSet) -> Either String (Map Tag (Vector TypeEnv.SimpleType))
  convertNodeSet (n, a@(NodeSet ns)) = mapM (checkNode n a <=< mapM (convertNodeItem . Set.toList)) ns
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70

  convertTypeSet :: (Name, TypeSet) -> Either String TypeEnv.Type
  convertTypeSet (name, ts) = do
    let ns = ts^.nodeSet
        st = ts^.simpleType
    case (Set.size st, Map.size $ ns^.nodeTagMap) of
<<<<<<< HEAD
      (stCount,nsCount) | stCount == 0 && nsCount > 0 -> TypeEnv.T_NodeSet <$> convertNodeSet ns
      (stCount,nsCount) | stCount > 0 && nsCount == 0 -> TypeEnv.T_SimpleType <$> convertSimpleTypeSet ns (Set.toList st)
=======
      (stCount,nsCount) | stCount == 0 && nsCount > 0 -> TypeEnv.T_NodeSet <$> convertNodeSet (name, ns)
      (stCount,nsCount) | stCount > 0 && nsCount == 0 -> TypeEnv.T_SimpleType <$> convertNodeItem (Set.toList st)
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
      (0,0)                                           -> pure TypeEnv.dead_t
      _ -> throwError $ printf "illegal type %s for %s" (show . pretty $ ts) name

  convertFunction :: (Name, (TypeSet, Vector TypeSet)) -> Either String (TypeEnv.Type, Vector TypeEnv.Type)
  convertFunction (name, (ret, args)) = (,) <$> convertTypeSet (name, ret) <*> mapM (convertTypeSet . (,) name) args

  typeEnv :: Either String TypeEnv.TypeEnv
  typeEnv = TypeEnv.TypeEnv <$>
    (mapM convertNodeSet  $ (\v -> V.zip (V.map (("Loc " <>) . packName . show) (V.iterateN (V.length v) succ 0)) v) $ (_memory hptResult)) <*>
    (mapM convertTypeSet  $ Map.mapWithKey (,) $ (_register hptResult)) <*>
    (mapM convertFunction $ Map.mapWithKey (,) $ (_function hptResult)) <*>
    pure (_sharing hptResult)

-- TODO: Add mode as a parameter?
inferTypeEnv :: Exp -> TypeEnv.TypeEnv
inferTypeEnv exp = either error id $ typeEnvFromHPTResult =<< result where
  hptProgram = HPT.codeGen exp
<<<<<<< HEAD
  hptResult = (R.evalDataFlowInfo . getDataFlowInfo) <$> hptProgram
  result = HPT.toHPTResult <$>  hptProgram <*> hptResult
=======
  hptResult = HPT.evalHPT <$> hptProgram
  result = HPT.toHPTResult <$>  hptProgram <*> (fst <$> hptResult)
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
