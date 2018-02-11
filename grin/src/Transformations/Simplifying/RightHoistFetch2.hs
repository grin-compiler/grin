{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor #-}
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

      -- the latter can be handled in generic way
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
rightHoistFetch :: Exp -> Exp
rightHoistFetch e = trace (ppShow globalFetchMap) $ ana builder ([], mempty, e)
  where
    globalFetchMap = collectFetchVars e

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