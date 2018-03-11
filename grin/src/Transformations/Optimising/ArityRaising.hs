{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Transformations.Optimising.ArityRaising where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin
import TypeEnv


{-
  TODO:
    - load ; no arity change
    - untag (if possible) ; increments arity

  done - examine defintions of function arguments in all callers
  - examine uses of function parameters inside all callees
  - do the actual transformation
-}
arityRaising :: Exp -> Exp
arityRaising = id

candidate :: TypeEnv -> Exp -> Set Name
candidate typeEnv@TypeEnv{..} = cata folder where

  folder :: ExpF (Set Name) -> Set Name
  folder = \case
    SAppF name args
      | not (isPrimName name)
      , locations <- mapMaybe getLocation args
      , not (null locations)
      , all singleTagLoc locations -> Set.singleton name
    exp -> Data.Foldable.fold exp

  getLocation :: Val -> Maybe [Int]
  getLocation (Var name) = case variableType typeEnv name of
    T_SimpleType T_Location{..} -> Just _locations
    _ -> Nothing
  getLocation _ = Nothing

  -- HINT: all location have the same single tag
  singleTagLoc :: [Int] -> Bool
  singleTagLoc (loc:locs) = Map.size (_location V.! loc) == 1 && all (==loc) locs
