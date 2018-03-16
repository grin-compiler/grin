{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Transformations.Simplifying.Vectorisation2 (vectorisation) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import Data.Monoid hiding (Alt)
import Data.Functor.Foldable as Foldable

import Text.PrettyPrint.ANSI.Leijen (pretty)

import Text.Printf

import Grin
import TypeEnv

import Control.Monad.State

type VectorisationAccumulator = (Map.Map Name Val, Exp)

getVarNodeArity :: TypeEnv -> Name -> Maybe Int
getVarNodeArity typeEnv@TypeEnv{..} name = case Map.lookup name _variable of
  Nothing -> error $ printf "getVarNodeArity - unknown variable %s" name
  Just (T_SimpleType _) -> Nothing
  Just (T_NodeSet ns)   -> Just $ maximum [1 + V.length args | args <- Map.elems ns]

vectorisation :: (TypeEnv, Exp) -> (TypeEnv, Exp)
vectorisation (typeEnv, expression) = (typeEnv, ana folder (Map.empty, expression))
  where
    folder :: VectorisationAccumulator -> ExpF VectorisationAccumulator
    folder (nameStore, expression) =
      case expression of
        EBind simpleexp var@(Var name) exp -> case getVarNodeArity typeEnv name of
          Nothing           -> EBindF (nameStore, simpleexp) var (nameStore, exp)
          Just maximumArity -> EBindF (nameStore, simpleexp) nodeContents (newNameStore, exp)
           where
            nodeContents = VarTagNode (name <> show 0) (map (\i -> Var (name <> show i)) [1 .. maximumArity])
            newNameStore = Map.insert name nodeContents nameStore
        ECase (Var name) alts | Just nodeContents <- Map.lookup name nameStore
          -> ECaseF nodeContents (map (\subExpression -> (nameStore, subExpression)) alts)
        SApp name vals -> SAppF name (map replaceVar vals)
        SReturn (Var name) | Just nodeContents <- Map.lookup name nameStore -> SReturnF nodeContents
        SStore (Var name) | Just nodeContents <- Map.lookup name nameStore -> SStoreF nodeContents
        SUpdate updateName (Var name) | Just nodeContents <- Map.lookup name nameStore -> SUpdateF updateName nodeContents
        e -> forwardRecursion
      where
        replaceVar (Var name) | Just val <- Map.lookup name nameStore = val
        replaceVar var = var
        forwardRecursion = fmap (\subExpression -> (nameStore, subExpression)) (project expression)

{-
  implement in two pass
  - collect / cata
    - new variables / name generator
    - update type env
    - collect substitution map

  - build
    - substitute vars with node
-}

data Info -- analysis info
  = Info
  { fetchVars   :: Map Name Name -- caseVar -> fetchVar ; MANY - ONE
  , caseVars    :: Set Name
  }
  deriving Show

emptyInfo = Info mempty mempty

type M a = State Info a

{-
  TODO:
    - pass type env
-}

collectFetchVars2 :: Exp -> (Exp, Info)
collectFetchVars2 = flip runState emptyInfo . cata collect where
  collect :: ExpF (M Exp) -> M Exp
  collect = \case
    --EBindF left var@(Var name) right -> pure $ EBind left var right
    e -> embed <$> sequence e
