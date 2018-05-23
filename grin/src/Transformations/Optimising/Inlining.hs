{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.Inlining where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin
import TypeEnv
import Transformations.Util
import Transformations.Names

-- analysis

data Stat
  = Stat
  { bindCount         :: !Int
  , functionCallCount :: !(Map Name Int)
  }

instance Monoid Stat where
  mempty = Stat 0 mempty
  mappend (Stat i1 m1) (Stat i2 m2) = Stat (i1 + i2) (Map.unionWith (+) m1 m2)

defStatistics :: Exp -> Stat
defStatistics = cata folder where
  folder :: ExpF Stat -> Stat
  folder = \case
    EBindF left _ right -> mconcat [left, right, Stat 1 mempty]
    SAppF name _        -> Stat 0 $ Map.singleton name 1
    exp -> Data.Foldable.fold exp

-- transformation

-- TODO: add the cloned variables to the type env
-- QUESTION: apo OR ana ???
inlining :: Set Name -> (TypeEnv, Program) -> (TypeEnv, Program)
inlining functionsToInline (typeEnv, prog@(Program defs)) = (typeEnv, evalNameM $ apoM builder prog) where

  defMap :: Map Name Def
  defMap = Map.fromList [(name, def) | def@(Def name _ _) <- defs]

  builder :: Exp -> NameM (ExpF (Either Exp Exp))
  builder = \case

    -- HINT: do not touch functions marked to inline
    Def name args body | Set.member name functionsToInline -> pure . DefF name args $ Left body

    -- HINT: bind argument values to function's new arguments and append the body with the fresh names
    --       with this solution the name refreshing is just a name mapping and does not require a substitution map
    SApp name argVals
      | Set.member name functionsToInline
      , Just def <- Map.lookup name defMap
      -> do
        (Def _ argNames funBody, nameMap) <- refreshNames mempty def
        let bind (n,v) e = EBind (SReturn v) (Var n) e
        pure . SBlockF . Left $ foldr bind funBody (zip argNames argVals)

    exp -> pure (Right <$> project exp)

{-
  - maintain type env
  - test inlining
  - test inline selection
  - test inline: autoselection + inlining

-}

inlineEval :: (TypeEnv, Exp) -> (TypeEnv, Exp)
inlineEval = inlining (Set.singleton "eval")

inlineApply :: (TypeEnv, Exp) -> (TypeEnv, Exp)
inlineApply = inlining (Set.singleton "apply")

lateInlining :: (TypeEnv, Exp) -> (TypeEnv, Exp)
lateInlining = inlining (Set.singleton "upto") -- TODO: use proper selection
