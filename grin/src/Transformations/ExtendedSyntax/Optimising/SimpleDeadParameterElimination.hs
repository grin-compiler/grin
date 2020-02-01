{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.ExtendedSyntax.Optimising.SimpleDeadParameterElimination where

import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Util

collectUsedNames :: Exp -> Set Name
collectUsedNames = cata folder where
  folder exp = foldNameUseExpF Set.singleton exp `mappend` Data.Foldable.fold exp

simpleDeadParameterElimination :: Program -> Program
simpleDeadParameterElimination prog@(Program exts defs) = ana builder prog where
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

    -- TODO: change this
    EBind leftExp (AsPat tag args var) rightExp
      | Tag kind tagName <- tag
      , isPFtag kind
      , Just deadIxs <- Map.lookup tagName deadArgMap
      -> EBindF leftExp (AsPat tag (removeDead deadIxs args) var) rightExp

    Alt cpat@NodePat{} altName body
      -> AltF (pruneCPat cpat) altName body

    exp -> project exp

  pruneVal :: Val -> Val
  pruneVal = \case
    ConstTagNode tag@(Tag kind name) args
      | isPFtag kind
      , Just dead <- Map.lookup name deadArgMap
      -> ConstTagNode tag (removeDead dead args)
    val -> val

  pruneCPat :: CPat -> CPat
  pruneCPat = \case
    NodePat tag@(Tag kind name) vars
      | isPFtag kind
      , Just deadIxs <- Map.lookup name deadArgMap
      -> NodePat tag (removeDead deadIxs vars)
    cpat -> cpat

  isPFtag :: TagType -> Bool
  isPFtag = \case
    F{} -> True
    P{} -> True
    _   -> False
