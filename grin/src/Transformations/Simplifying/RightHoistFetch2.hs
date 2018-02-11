{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor, TemplateHaskell #-}
module Transformations.Simplifying.RightHoistFetch2 where

import Debug.Trace (traceShowId, trace)
import Text.Show.Pretty (ppShow)
import Text.Printf

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Lens.Micro.Platform

import Grin

-- alernative monoid instance for Map
newtype MonoidMap k v = MonoidMap {monoidMap :: Map k v} deriving (Functor, Show)
instance (Monoid v, Ord k) => Monoid (MonoidMap k v) where
  mempty = MonoidMap mempty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap $ Map.unionWith mappend a b

data Step
  = SBindL  Int -- ^ duplicate count
  | SBindR  Int -- ^ duplicate count
  | SAlt    Int -- ^ case alternative index
  deriving (Eq, Ord, Show)

type VarMap = MonoidMap Name (Set [Step]) -- set of alt traces

data RHF
  = RHF
  { defMap        :: Map Name [Step] -- name -> def scope
  , occurrenceMap :: VarMap
  , globalMap     :: Map Name VarMap
  }
  deriving Show

instance Monoid RHF where
  mempty = RHF mempty mempty mempty
  mappend (RHF a1 b1 c1) (RHF a2 b2 c2) = RHF (a1 `mappend` a2) (b1 `mappend` b2) (c1 `mappend` c2)

{-
  TODO
    - fold referred var names
-}

type FetchVarMap = Map Name [[Step]] -- list of use site scopes

{-
  all prefix == current scope ; live variable
  some prefix <> current scope ; impossible, this fetch must be emitted earlier and deleted from the map
  exp uses var and current scope in the scope list ; first use, emit fetch, substitute references
-}
-- collect all indexed fetch bouded variables and their use sites
collectFetchVars :: Exp -> Map Name FetchVarMap
collectFetchVars = fmap (fmap Set.toList . monoidMap) . globalMap . para collect where
  collect :: ExpF (Exp, RHF) -> RHF
  collect = \case
    EBindF (leftExp, left) (Var name) (_, right) ->
        mconcat $ [ prefixStep (SBindL 1) left
                  , prefixStep (SBindR 1) right
                  ] ++ case leftExp of
                        SFetchI _ Just{} -> [addFetch name]
                        _ -> []

    expRHF -> case fmap snd expRHF of
      DefF name _args x@RHF{..} -> trace ("unfiltered " ++ ppShow x) $ 
              RHF
                { defMap        = mempty
                , occurrenceMap = mempty
                , globalMap     = Map.singleton name fetchVarMap
                } where
                    -- keep only fetch variables
                    fetchVarMap = MonoidMap . Map.filterWithKey (\n _ -> Map.member n defMap) $ monoidMap occurrenceMap

      ECaseF val alts   -> mconcat $ useNameVal val : zipWith (prefixStep . SAlt) [0..] alts

      -- the rest can be handled in generic way
      SAppF _name args  -> mconcat $ map useNameVal args
      SReturnF val      -> useNameVal val
      SStoreF val       -> useNameVal val
      SFetchIF name pos -> useName name
      SUpdateF name val -> mconcat [useName name, useNameVal val]
      e -> Data.Foldable.fold e

  addFetch n = RHF (Map.singleton n []) mempty mempty
  collectNames = foldNames pure
  useNameVal = mconcat . map useName . collectNames
  useName n = RHF mempty (MonoidMap $ Map.singleton n (Set.singleton [])) mempty

prefixStep :: Step -> RHF -> RHF
prefixStep step rhid@RHF{..} = rhid
  { occurrenceMap = fmap (Set.map add) occurrenceMap
  , defMap        = fmap add defMap
  }
  where add [] = [step]
        add (x:xs) = case (step, x) of
          (SBindL a, SBindL b) -> SBindL (a + b) : xs
          (SBindR a, SBindR b) -> SBindR (a + b) : xs
          _ -> step : x : xs


{-
  actions:
    live = all (isPrefix scope) scopes
      is it in live map and used now? YES: emit fetch + add to substitution map + remove from live map NO: go on
      for case
        is it referred in all alternatives?
          can be define here ? YES: define here NO: go on
        is it referred in some alternatives?
          remove from alternative live maps where it is not referred
          
    dead = not $ any (isPrefix scope) scopes ; remove
-}
_rightHoistFetch :: Exp -> Exp
_rightHoistFetch e = trace (printf "old:\n%s\nnew:\n%s" (ppShow globalFetchMap) (ppShow globalFetchMap2)) $ ana builder ([], mempty, e)
  where
    globalFetchMap = collectFetchVars e
    globalFetchMap2 = collectFetchVars2 e

    builder :: ([Int], FetchVarMap, Exp) -> ExpF ([Int], FetchVarMap, Exp)
    builder (path, liveVars, exp) = case exp of
      Def name args body -> DefF name args ([], globalFetchMap Map.! name, body)

      -- Remove original fetch
      -- Must emit some code, apo does not have a skip command
      EBind fetch@(SFetchI locVarName (Just{})) (Var name) rightExp -- TODO: locVarName could be substituted, store fetch command here
          | Map.member name liveVars -> EBindF (path, liveVars, SReturn Unit) Unit (path, liveVars, rightExp)

      -- filter out divergent paths
      ECase val alts -> ECaseF val [(path ++ [i],liveVars,alt) | (i,alt) <- zip [0..] alts]

      _ -> (path,liveVars,) <$> project exp
{-
  NOTE: move fetches to the first use
-}

-- ana :: (a -> Base t a) -> a -> t 

{-
  NOTE: for the phd grin the var lpat is the only relevant option for indexed fetch
  IDEAS:
    - build var dependency tree for fetch vars i.e. varChild <- fetch parentVar[i]
    - full traversal is necessary due to substitution ; building a var dependency graph would be more expensive
    - mapVarNames, foldVarNames for Val and Exp (non recursive)
    - 
-}

------------------

{-
  NAMING:
    caseVar <- fetch fetchVar[0]
    itemVar <- fetch fetchVar[1] ; index > 0
    case caseVar of

  New and simplified algorithm
    - collect caseVar and fetchVar
    - move every non tagged indexed fetch under the corresponding case and tag them according the alternative
    - use the tag info to determine the tag arity and move only the relevant fetch operation

  RESTRICTION:
    - values come from untagged indexed fetches must not be used before the corresponding case
      otherwise it is ERROR or the operations have to be MOVED under the case along with the fetches

  HINT:
    - do not care about name aliases, that is responsibility of other transformations
    - ignore the variable liveness, the dead variable eliminiation will get rid of it
-}
data RHF2
  = RHF2
  { fetchVars   :: Map Name Name -- caseVar -> fetchVar ; MANY - ONE
  , caseVars    :: Set Name
  , globalMap2  :: Map Name (Set Name) -- def -> Set fetchVar
  }
  deriving Show

--concat <$> mapM makeLenses [''NodeSet, ''Value, ]

instance Monoid RHF2 where
  mempty = RHF2 mempty mempty mempty
  mappend (RHF2 a1 b1 c1) (RHF2 a2 b2 c2) = RHF2 (a1 `mappend` a2) (b1 `mappend` b2) (c1 `mappend` c2)

{-
  QUESTION:
    What about fetch var dependency for non tag vars?
  ANSWER:
    Hoist every non tag fetch under the corresponding case because every fetch must be tagged.
    This is a codegen requirement.
-}

--collectFetchVars2 :: Exp -> Map Name (Set Name)
collectFetchVars2 :: Exp -> Set Name
collectFetchVars2 = cull . para collect where
  collect :: ExpF (Exp, RHF2) -> RHF2
  collect = \case
    EBindF (SFetchI fetchVar (Just 0), left) (Var caseVar) (_, right) -> mconcat [right, addFetchVar caseVar fetchVar]

    expRHF -> case fmap snd expRHF of
      ECaseF (Var caseVar) alts -> mconcat $ addCaseVar caseVar : alts
{-
      DefF name _args RHF2{..} -> addGlobal name fetchVarSet
                where -- keep only fetch variables that have corresponding case
                      fetchVarSet = Set.fromList . Map.elems $ Map.filterWithKey (\n _ -> Set.member n caseVars) fetchVars
-}
      e -> Data.Foldable.fold e

  addCaseVar caseVar = RHF2 mempty (Set.singleton caseVar) mempty
  addFetchVar caseVar fetchVar = RHF2 (Map.singleton caseVar fetchVar) mempty mempty
  --addGlobal name fetchVarSet = RHF2 mempty mempty (Map.singleton name fetchVarSet)

  cull RHF2{..} = Set.fromList . Map.elems $ Map.filterWithKey (\n _ -> Set.member n caseVars) fetchVars

{-
  FIRST VERSION:
    - no fetch var path filtering
    - move all fetches under the case, delete them when moved
    - substitute names
-}

data RHF3
  = RHF3
  { _path       :: [Int]
  , _hoistMap   :: Map Name [(Name, Int)] -- fetchVar -> [(itemVar, fetchIndex)]
  , _caseMap    :: Map Name Name          -- caseVar  -> fetchVar
  }
  deriving Show

concat <$> mapM makeLenses [''RHF3]

emptyRHF3 = RHF3 mempty mempty mempty

rightHoistFetch :: Exp -> Exp
rightHoistFetch e = trace (printf "fetch vars:\n%s" (ppShow globalFetchMap)) $ ana builder ([], mempty, e)
  where
    globalFetchMap = collectFetchVars2 e

    builder :: ([Int], Map Name [(Name, Int)], Exp) -> ExpF ([Int], Map Name [(Name, Int)], Exp)
    builder (path, hoistMap, exp) = case exp of
      --Def name args body -> DefF name args ([], globalFetchMap Map.! name, mempty, body)

      -- Remove original fetch
      -- Must emit some code, apo does not have a skip command
      -- TODO: locVarName could be substituted, store fetch command here
      EBind fetch@(SFetchI fetchVar (Just idx)) lpat@(Var caseVar) rightExp
          | Set.member fetchVar globalFetchMap -> case idx of
              -- TODO: save caseVar for case recognition
              0 -> EBindF (path, hoistMap, fetch) lpat (path, hoistMap, rightExp)
              _ -> EBindF (path, hoistMap, SReturn Unit) Unit (path, hoistMap, rightExp)

      -- filter out divergent paths
      ECase val alts -> ECaseF val [(path ++ [i], hoistMap, alt) | (i,alt) <- zip [0..] alts]

      _ -> (path,hoistMap,) <$> project exp
