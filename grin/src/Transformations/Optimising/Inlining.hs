{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.Inlining where

import Text.Printf
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin
import TypeEnv
import Transformations.Util

{-
  collect function statistics:
    done - bind count
    done - called functions + count
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
  done - substitute the arguments with parameters
  done - substitute all binding names with fresh names ; separate builder
  - generate name suffix
  - add the cloned variables to the type env
-}


inlining :: TypeEnv -> Set Name -> Program -> Program
inlining typeEnv functionsToInline prog@(Program defs) = apo builder prog where
  defMap :: Map Name Def
  defMap = Map.fromList [(name, def) | def@(Def name _ _) <- defs]

  lookupDef :: Name -> Def
  lookupDef name = case Map.lookup name defMap of
    Nothing -> error $ printf "unknown function: %s" name
    Just a  -> a

  builder :: Exp -> ExpF (Either Exp Exp)
  builder = \case
    -- HINT: do not touch functions marked to inline
    Def name args body | Set.member name functionsToInline -> DefF name args $ Left body

    -- HINT: bind argument values to function's new arguments and append the body with the fresh names
    --       with this solution the name refreshing is just a name mapping and does not require a substitution map
    SApp name vals | Set.member name functionsToInline -> SBlockF . Left $ bindArgs newBody (zip vals newArgs) where
      Def _ args body = lookupDef name

      mapName :: Name -> Name
      mapName = (++ nameSuffix)

      nameSuffix      = ".inlined" -- TODO: make this always unique
      newArgs         = map mapName args
      newBody         = mapNames mapName body

    exp -> Right <$> project exp

  bindArgs :: Exp -> [(Val, Name)] -> Exp
  bindArgs = foldr (\(val, name) exp -> EBind (SReturn val) (Var name) exp)

-- map variable and binding names

mapNames :: (Name -> Name) -> Exp -> Exp
mapNames mapName = ana builder where
  builder :: Exp -> ExpF Exp
  builder = project . mapVarBindExp mapName . mapVarRefExp mapName
