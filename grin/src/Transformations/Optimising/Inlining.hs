{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.Inlining where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin
import TypeEnv
import Transformations.Util

{-
  collect function statistics:
    - bind count
    - called functions + count
-}

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

{-
  done - replace SApp-s with Block with the function body
  - substitute the arguments with parameters
  - substitute all binding names with fresh names ; separate builder
-}
refreshBinds = undefined
getFunctionBody = undefined

inlining :: TypeEnv -> Set Name -> Exp -> Exp
inlining typeEnv functionsToInline = apo builder where

  builder :: Exp -> ExpF (Either Exp Exp)
  builder = \case
    -- HINT: do not touch functions marked to inline
    Def name args body | Set.member name functionsToInline -> DefF name args $ Left body

    SApp name args | Set.member name functionsToInline -> SBlockF . Left . refreshBinds $ getFunctionBody name
    exp -> Right <$> project exp
