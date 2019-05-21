{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.SimpleDeadVariableElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Grin.Grin
import Grin.TypeEnv
import Grin.EffectMap
import Transformations.Util
import Lens.Micro.Platform


-- TODO: Write for dead code elimination.???
-- TODO: Remove TypeEnv, consult EffectMap for side-effects, dont rely on unit return type
-- QUESTION: should SDVE use any interprocedural information?
simpleDeadVariableElimination :: TypeEnv -> EffectMap -> Exp -> Exp
simpleDeadVariableElimination typeEnv effMap e = cata folder e ^. _1 where

  effectfulExternals :: Set Name
  effectfulExternals = case e of
    Program es _ -> Set.fromList $ map eName $ filter eEffectful es
    _            -> Set.empty

  folder :: ExpF (Exp, Set Name, Bool) -> (Exp, Set Name, Bool)
  folder = \case

    exp@(EBindF (left, _, True) lpat right) -> embedExp exp
    exp@(EBindF (left, _, _) lpat right@(_, rightRef, _))
      | lpat /= Unit
      , vars <- foldNamesVal Set.singleton lpat       -- if all the variables
      , all ((/=) unit_t . variableType typeEnv) vars -- which does not hol unit
      , all (flip Set.notMember rightRef) vars        -- and are not referred
      -> case left of
          SBlock{}  -> embedExp exp
          _         -> right

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
