{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, TemplateHaskell #-}

module Reducer.LLVM.InferType where

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


import Reducer.LLVM.Base
import AbstractInterpretation.HPTResultNew
import AbstractInterpretation.PrettyHPT ()
import Grin
import Pretty ()


-- type construction for values

simpleTypeTypeSet :: SimpleType -> TypeSet
simpleTypeTypeSet sTy = TypeSet (Set.singleton . SimpleType $ sTy) mempty

-- allows simple type singletons or locations
validateNodeItem :: TypeSet -> CG ()
validateNodeItem ts
  | Map.null (ts^.nodeSet.nodeTagMap) &&
    Set.size stlS > 0 &&
    (  Set.foldl' (\a v -> a && isLocation v) True stlS
    || Set.size stlS == 1 && not (isLocation $ Set.findMin stlS)
    ) = pure ()
  | otherwise = fail $ printf "illegal node item type %s" (show $ pretty ts)
  where
    stlS = ts^.simpleTypeAndLocationSet
    isLocation = \case
      Location{} -> True
      _          -> False

nodeTypeSet :: Tag -> [TypeSet] -> CG TypeSet
nodeTypeSet tag items = do
  mapM_ validateNodeItem items
  pure $ TypeSet mempty $ NodeSet $ Map.singleton tag $ V.fromList $ map _simpleTypeAndLocationSet items

typeOfLit :: Lit -> TypeSet
typeOfLit lit = simpleTypeTypeSet $ case lit of
  LInt64{}  -> T_Int64
  LWord64{} -> T_Word64
  LFloat{}  -> T_Float
  LBool{}   -> T_Bool

typeOfVal :: Val -> CG TypeSet
typeOfVal val = do
  case val of
    ConstTagNode tag args -> mapM typeOfVal args >>= nodeTypeSet tag
    {-
    VarTagNode    Name [SimpleVal] -- complete node (variable tag)
    ValTag        Tag
    -}
    Unit      -> pure $ simpleTypeTypeSet T_Unit
    Lit lit   -> pure $ typeOfLit lit
    Var name  -> use (envHPTResult.register.at name) >>= \case
                  Nothing -> error $ printf "unknown variable %s" name
                  Just ty -> pure ty
    _ -> error $ printf "unsupported val" (show $ pretty val)
