{-# LANGUAGE LambdaCase #-}
module Transformations.ExtendedSyntax.Optimising.DeadVariableElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Lens.Micro.Platform

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TypeEnv
import Grin.ExtendedSyntax.EffectMap
import Transformations.ExtendedSyntax.Util


-- TODO: consult EffectMap for side-effects
-- QUESTION: should DVE use any interprocedural information? [1]
deadVariableElimination :: EffectMap -> Exp -> Exp
deadVariableElimination effMap e = cata folder e ^. _1 where

  effectfulExternals :: Set Name
  effectfulExternals = case e of
    Program es _ -> Set.fromList $ map eName $ filter eEffectful es
    _            -> Set.empty

  folder :: ExpF (Exp, Set Name, Bool) -> (Exp, Set Name, Bool)
  folder = \case

    exp@(EBindF (left, _, True) bPat right) -> embedExp exp
    exp@(EBindF (left, _, _) bPat right@(_, rightRef, _))
      | vars <- foldNames Set.singleton bPat          -- if all the variables
      , all (flip Set.notMember rightRef) vars        -- are not referred
      -> case left of
          SBlock{}  -> embedExp exp
          SUpdate{} -> embedExp exp
          _         -> right

    -- QUESTION: Should we just keep all function calls? See [1]
    exp@(SAppF name _) ->
      embedExp exp & _3 .~ (hasPossibleSideEffect name effMap || Set.member name effectfulExternals)

    exp -> embedExp exp
    where
      embedExp :: ExpF (Exp, Set Name, Bool) -> (Exp, Set Name, Bool)
      embedExp exp0 =
        ( embed (view _1 <$> exp0)
        , foldNameUseExpF Set.singleton exp0 `mappend` Data.Foldable.fold (view _2 <$> exp0)
        , getAny $ Data.Foldable.fold (view (_3 . to Any) <$> exp0)
        )
