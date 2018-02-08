{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Transformations.Simplifying.Vectorisation where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid hiding (Alt)
import Data.Functor.Foldable as Foldable

import Text.Printf

import Grin
import AbstractInterpretation.HPTResult (HPTResult(..), Computer(..), RTVar(..), RTNode(..))

type VectorisationAccumulator = (Map.Map Name Val, Exp)

getVarNodeArity :: HPTResult -> Name -> Maybe Int
getVarNodeArity Computer{..} name = case Map.lookup name envMap of
  Nothing -> error $ printf "getVarNodeArity - unknown variable '%s'" name
  Just varSet -> case [length args | N (RTNode tag args) <- Set.toList varSet] of
    [] -> Nothing
    maxArityWithoutTag -> Just $ maximum maxArityWithoutTag

vectorisation :: HPTResult -> Exp -> Exp
vectorisation hptResult expression = apo folder (Map.empty, expression)
  where
    folder :: VectorisationAccumulator -> ExpF (Either Exp VectorisationAccumulator)
    folder (nameStore, expression) =
      case expression of
        EBind simpleexp var@(Var name) exp -> case getVarNodeArity hptResult name of
          Nothing           -> EBindF (Right (nameStore, simpleexp)) var (Right (nameStore, exp))
          Just maximumArity -> EBindF (Right (nameStore, simpleexp)) nodeContents (Right (newNameStore, exp))
           where
            nodeContents = VarTagNode (name <> show 0) (map (\i -> Var (name <> show i)) [1 .. maximumArity])
            newNameStore = Map.insert name nodeContents nameStore
        ECase (Var name) alts | Just nodeContents <- Map.lookup name nameStore -> ECaseF nodeContents (map (\subExpression -> Right (nameStore, subExpression)) alts)
        SApp name vals -> SAppF name (map replaceVar vals)
        SReturn (Var name) | Just nodeContents <- Map.lookup name nameStore -> SReturnF nodeContents
        SStore (Var name) | Just nodeContents <- Map.lookup name nameStore -> SStoreF nodeContents
        SUpdate updateName (Var name) | Just nodeContents <- Map.lookup name nameStore -> SUpdateF updateName nodeContents
        e -> forwardRecursion
      where
        replaceVar (Var name) | Just val <- Map.lookup name nameStore = val
        replaceVar var = var
        forwardRecursion = fmap (\subExpression -> Right (nameStore, subExpression)) (project expression)
