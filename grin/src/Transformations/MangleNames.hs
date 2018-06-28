{-# LANGUAGE RecordWildCards #-}
module Transformations.MangleNames where

import Text.Printf
import Data.Functor.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State

import Transformations.Util
import Transformations.Names

import Grin.Grin

data Env
  = Env
  { counter :: Int
  , nameMap :: Map Name Name
  }

type M = State Env

mangleNames :: Exp -> Exp
mangleNames e = evalState (anaM builder e) (Env 0 mempty) where

  builder :: Exp -> M (ExpF Exp)
  builder = fmap project . mapNameDefExpM defName <=< mapNameUseExpM useName

  defName :: Name -> M Name
  defName name = state $ \env@Env{..} ->
    let new = printf "name.%d" counter
    in (new, env {counter = succ counter, nameMap = Map.insert name new nameMap})

  useName :: Name -> M Name
  useName n = Map.findWithDefault n n <$> gets nameMap
