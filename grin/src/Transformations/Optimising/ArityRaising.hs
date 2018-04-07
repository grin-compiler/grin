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
import Debug.Trace
import Lens.Micro.Platform
import qualified Data.Vector as Vector
import Control.Monad
import Data.Monoid
import Data.Functor.Infix
import Data.List


{-
  TODO:
    - load ; no arity change
    - untag (if possible) ; increments arity

  done - examine defintions of function arguments in all callers
  - examine uses of function parameters inside all callees
  - do the actual transformation
-}
arityRaising :: (TypeEnv, Exp) -> (TypeEnv, Exp)
arityRaising (te, exp) = (te, exp)

{-
Step 1: Examine the function parameter types
Step 2: Examine the callees
Step 3: Transform
-}

-- Return the functions that has node set parameters with unique names
examineTheParameters :: (TypeEnv, Exp) -> Map Name [(Name, NodeSet)]
examineTheParameters (te, e) = Map.filter (not . null) $ Map.map candidate funs
  where
    funs :: Map Name [(Name, Type)]
    funs = Map.intersectionWith combine (_function te) funParamNames
    combine (_tr, pt) params = params `zip` (Vector.toList pt)

    funParamNames = flip cata e $ \case
      ProgramF defs      -> mconcat defs
      DefF name params _ -> Map.singleton name params
      _                  -> mempty

    candidate :: [(Name, Type)] -> [(Name, NodeSet)]
    candidate = mapMaybe $ \(name, typ) -> (,) name <$>
      typ ^? _T_SimpleType
           . _T_Location
           . to (sameNodeOnLocations te)
           . _Just

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

-- Keep the parameters that are part of an invariant calls,
-- or an argument to a fetch.
examineCallees :: Map Name [(Name, NodeSet)] -> (TypeEnv, Exp) -> Map Name [(Name, NodeSet)]
examineCallees funParams (te, exp) =
    Map.mapMaybe (nonEmpty . (filter ((`Set.notMember` others) . fst))) funParams
  where
    others = cata collect exp

    vars :: Val -> Set Name
    vars = Set.fromList . \case
      Var n             -> [n]
      ConstTagNode _ vs -> vs ^.. each . _Var
      VarTagNode n vs   -> n : (vs ^.. each . _Var)
      _                 -> []

    collect :: ExpF (Set Name) -> Set Name
    collect = \case
      ProgramF  defs             -> mconcat defs
      DefF      name params body -> body
      SBlockF   body             -> body
      EBindF    lhs _ rhs        -> lhs <> rhs
      ECaseF    val as           -> mconcat as
      AltF cpat body             -> body

      SReturnF  val      -> vars val
      SStoreF   val      -> vars val
      SUpdateF  name val -> Set.insert name (vars val)
      _ -> mempty

sameNodeOnLocations :: TypeEnv -> [Int] -> Maybe NodeSet
sameNodeOnLocations te is = join $ allSame $ map (oneNodeOnLocation te) is

oneNodeOnLocation :: TypeEnv -> Int -> Maybe NodeSet
oneNodeOnLocation te idx = case (Map.size ns) of
  1 -> Just ns
  _ -> Nothing
  where
    ns = (_location te) Vector.! idx

allSame :: (Eq a) => [a] -> Maybe a
allSame []     = Nothing
allSame [a]    = Just a
allSame (a:as) = if all (a==) as then Just a else Nothing


{-
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
-}
