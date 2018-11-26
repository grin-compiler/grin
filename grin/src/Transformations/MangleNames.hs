{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
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

{- NOTE: We need two seperate traversals in order to
         first collect all names at definition sites,
         then to replace all names at use- and definition sites.
-}
mangleNames :: Exp -> Exp
mangleNames e = evalState (collectNames >=> replaceNames $ e) (Env 0 mempty) where

  -- collects the names from deifinition sites
  collectNames :: Exp -> M Exp
  collectNames = anaM coalg  where
    coalg :: Exp -> M (ExpF Exp)
    coalg = fmap project . mapNameDefExpM defName

  -- replaces names at use- and deifinition sites
  replaceNames :: Exp -> M Exp
  replaceNames = cataM alg  where
    alg :: ExpF Exp -> M Exp
    alg = (mapNameUseExpM useName >=> mapNameDefExpM useName) . embed

  defName :: Name -> M Name
  defName name = state $ \env@Env{..} ->
    let new = "name." <> showTS counter
    in (name, env {counter = succ counter, nameMap = Map.insert name new nameMap})

  useName :: Name -> M Name
  useName n = Map.findWithDefault n n <$> gets nameMap
