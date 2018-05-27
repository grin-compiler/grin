{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.DeadParameterElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin
import Transformations.Util

collectUsedNames :: Exp -> Set Name
collectUsedNames = cata folder where
  folder exp = foldNameUseExpF Set.singleton exp `mappend` Data.Foldable.fold exp

deadParameterElimination :: Program -> Program
deadParameterElimination prog@(Program defs) = ana builder prog where
  deadArgMap :: Map Name (Set Int)
  deadArgMap = mconcat $ mapMaybe deadArgsInDef defs

  deadArgsInDef :: Def -> Maybe (Map Name (Set Int))
  deadArgsInDef def@(Def name args _)
    | usedNames       <- collectUsedNames def
    , deadArgIndices  <- Set.fromList . map fst . filter (flip Set.notMember usedNames . snd) $ zip [0..] args
    = if null deadArgIndices
        then Nothing
        else Just $ Map.singleton name deadArgIndices

  removeDead :: Set Int -> [a] -> [a]
  removeDead dead args = [arg | (idx, arg) <- zip [0..] args, Set.notMember idx dead]

  builder :: Exp -> ExpF Exp
  builder = \case
    Def name args body
      | Just dead <- Map.lookup name deadArgMap
      -> DefF name (removeDead dead args) body
    SApp name args
      | Just dead <- Map.lookup name deadArgMap
      -> SAppF name (removeDead dead args)
    {-
      implementation
        done - def
        done - app
        - Fname ; val, lpat, cpat
        - Pname
      test
        - def
        - app
        - Fname ; val, lpat, cpat
        - Pname
    -}
    exp -> project exp
