{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Transformations.Simplifying.Vectorisation2 (vectorisation) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad.State
import Lens.Micro.Platform
import Data.Functor.Foldable as Foldable

import Text.Printf

import Grin
import TypeEnv
import Transformations.Util

data Build
  = Build
  { _typeEnv  :: TypeEnv
  , _substMap :: Map Val Val
  }
  deriving Show

type M a = State Build a

vectorisation :: TypeEnv -> Exp -> (TypeEnv, Exp)
vectorisation typeEnv exp = (_typeEnv b, e) where
  (e, b) = runState (anaM builder exp) (Build typeEnv mempty)

  builder :: Exp -> M (ExpF Exp)
  builder = \case
    EBind leftExp var@(Var name) rightExp | T_NodeSet nodeSet <- variableType typeEnv name -> do
      let (varTypes, varTagNode) = createVarTagNode name nodeSet
      modify' $ \b@Build{..} -> b
        { _substMap = Map.insert var varTagNode _substMap
        , _typeEnv  = _typeEnv & variable %~ Map.union varTypes
        }
      pure $ EBindF leftExp varTagNode rightExp
    exp -> do
      env <- gets _substMap
      pure $ project (substVals env exp)


-- calculate tagged union mapping

data TUBuild
  = TUBuild
  { tubItemPool :: Map SimpleType [Int]
  , tubItems    :: Map SimpleType [Int]
  , tubLayout   :: [SimpleType]
  , tubSize     :: Int
  }

emptyTUBuild = TUBuild mempty mempty mempty 0

type TU = State TUBuild

createVarTagNode :: Name -> NodeSet -> (Map Name Type, Val)
createVarTagNode name nodeSet = (varTypes, varTagNode) where
  -- TODO: create ConstTagNode for singleton node set
  tagName    = printf "%s.tag" name
  varTypes   = Map.fromList $ (tagName, T_Tag nodeSet) : items
  varTagNode = VarTagNode tagName (map (Var . fst) items) tuMapping

  (tuMapping, tub) = runState (mapM mapNode nodeSet) emptyTUBuild
  items = [(printf "%s.%d" name idx, T_SimpleType sTy) | (idx, sTy) <- zip [(0 :: Int)..] $ reverse $ tubLayout tub]

  mapNode :: Vector SimpleType -> TU [Int]
  mapNode v = do
    modify $ \tub@TUBuild{..} -> tub {tubItemPool = tubItems}
    mapM allocIndex $ V.toList v

  allocIndex :: SimpleType -> TU Int
  allocIndex sTy = do
    pool <- gets tubItemPool
    size <- gets tubSize
    let idx = head $ Map.findWithDefault [size] sTy pool ++ [size]
    modify $ \tub@TUBuild{..} -> tub {tubItemPool = Map.adjust (drop 1) sTy tubItemPool}
    when (idx == size) $ do
      modify $ \tub@TUBuild{..} -> tub
        { tubItems  = Map.unionWith mappend tubItems $ Map.singleton sTy [size]
        , tubSize   = succ size
        , tubLayout = sTy : tubLayout
        }
    pure idx
