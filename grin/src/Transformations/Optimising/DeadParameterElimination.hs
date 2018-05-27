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
  builder e = case mapValsExp pruneVal e of
    Def name args body
      | Just dead <- Map.lookup name deadArgMap
      -> DefF name (removeDead dead args) body

    SApp name args
      | Just dead <- Map.lookup name deadArgMap
      -> SAppF name (removeDead dead args)

    EBind leftExp lpat@ConstTagNode{} rightExp
      -> EBindF leftExp (pruneVal lpat) rightExp

    Alt cpat@NodePat{} body
      -> AltF (pruneCPat cpat) body

    exp -> project exp

  pruneVal :: Val -> Val
  pruneVal = \case
    ConstTagNode tag@(Tag kind name) vals
      | isPFtag kind
      , Just dead <- Map.lookup name deadArgMap
      -> ConstTagNode tag (removeDead dead vals)
    val -> val

  pruneCPat :: CPat -> CPat
  pruneCPat = \case
    NodePat tag@(Tag kind name) vars
      | isPFtag kind
      , Just dead <- Map.lookup name deadArgMap
      -> NodePat tag (removeDead dead vars)
    cpat -> cpat

  isPFtag :: TagType -> Bool
  isPFtag = \case
    F{} -> True
    P{} -> True
    _   -> False