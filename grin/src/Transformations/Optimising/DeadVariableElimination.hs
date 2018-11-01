{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module Transformations.Optimising.DeadVariableElimination where

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec

import Data.List
import Data.Monoid

import qualified Data.Foldable
import Data.Functor.Foldable as Foldable

import Lens.Micro
import Lens.Micro.Platform

import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Trans.Except

import Grin.Grin
import Grin.Pretty
import Grin.TypeEnv
import Transformations.Util
import AbstractInterpretation.LVAUtil as LVA


data DeletedEntities = DeletedEntities 
  { _deVariables :: Set Name 
  , _deLocations :: Set Int 
  }
  deriving (Eq, Ord, Show)

instance Monoid (DeletedEntities) where 
  mempty = DeletedEntities mempty mempty 
  mappend (DeletedEntities vars1 locs1) (DeletedEntities vars2 locs2) = 
    DeletedEntities (vars1 <> vars2) (locs1 <> locs2)

concat <$> mapM makeLenses [''DeletedEntities]

type Trf = ExceptT String (State DeletedEntities)

runTrf :: Trf a -> Either String a 
runTrf = flip evalState mempty . runExceptT 

-- P and F nodes are handled by Dead Data Elimination
deadVariableElimination :: LVAResult -> TypeEnv -> Exp -> Either String Exp
deadVariableElimination lvaResult tyEnv 
  = runTrf . deleteDeadBindings lvaResult tyEnv

deleteDeadBindings :: LVAResult -> TypeEnv -> Exp -> Trf Exp 
deleteDeadBindings lvaResult tyEnv = cataM alg where 
  alg :: ExpF Exp -> Trf Exp 
  alg = \case 
    e@(EBindF SReturn{} lpat rhs) -> rmWhenAllDead e rhs lpat 
    e@(EBindF SFetchI{} lpat rhs) -> rmWhenAllDead e rhs lpat
    e@(EBindF (SStore _) (Var p) rhs) 
      | Just locs <- tyEnv ^? variable . at p . _Just . _T_SimpleType . _T_Location -> do
        unless (isSingleton locs) (throwE $ multipleLocs p locs)
        pointerDead <- isVarDeadM p 
        rmWhen pointerDead e rhs (Set.singleton p) (Set.fromList locs)
    e@(EBindF (SApp f _) lpat rhs) -> do 
      let names = foldNamesVal Set.singleton lpat
      funDead <- isFunDeadM f 
      rmWhen funDead e rhs names mempty
    e@(EBindF (SUpdate p v) Unit rhs) -> do 
      varDead <- isVarDeadM p 
      rmWhen varDead e rhs mempty mempty
    e -> pure . embed $ e

  rmWhenAllDead :: ExpF Exp -> Exp -> Val -> Trf Exp
  rmWhenAllDead orig modified val = do
    let names  = foldNamesVal Set.singleton val
    allVarsDead <- allM isVarDeadM . Set.toList $ names
    rmWhen allVarsDead orig modified names mempty

  rmWhen :: Bool -> ExpF Exp -> Exp -> Set Name -> Set Int -> Trf Exp 
  rmWhen needsRemoval orig modified variables locations
    | needsRemoval = do deVariables %= (mappend variables)
                        deLocations %= (mappend locations)
                        pure modified
    | otherwise = pure . embed $ orig 
  
  isVarDeadM :: Name -> Trf Bool 
  isVarDeadM v = fmap (not . isLive) 
                . lookupExcept (varLvNotFound v) v
                . _register
                $ lvaResult

  isFunDeadM :: Name -> Trf Bool 
  isFunDeadM f = fmap isFunDead 
               . lookupExcept (funLvNotFound f) f
               . LVA._function
               $ lvaResult

  varLvNotFound v = "DFE: Variable " ++ show (PP v) ++ " was not found in liveness map"
  funLvNotFound f = "DFE: Function " ++ show (PP f) ++ " was not found in liveness map"
    
  isSingleton :: [a] -> Bool 
  isSingleton [_] = True
  isSingleton _   = False

  multipleLocs :: Name ->  [Int] -> String 
  multipleLocs p locs = "DFE: A pointer bound out from a store instruction " 
                     ++ "should always point to a single locationn, "
                     ++ "but " ++ show (PP p) ++ " points to multiple locations: "
                     ++ show (PP locs)