{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor, TemplateHaskell #-}
module Transformations.Simplifying.RightHoistFetch2 (rightHoistFetch) where

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
import Transformations.Util

-- path tracking

data Step
  = SBindL  Int -- ^ duplicate count
  | SBindR  Int -- ^ duplicate count
  | SAlt    Int -- ^ case alternative index
  deriving (Eq, Ord, Show)


addStep :: Step -> [Step] -> [Step]
addStep step [] = [step]
addStep step (x:xs) = case (step, x) of
  (SBindL a, SBindL b) -> SBindL (a + b) : xs
  (SBindR a, SBindR b) -> SBindR (a + b) : xs
  _ -> step : x : xs

showPath :: [Step] -> Name
showPath path = concat (map f path) where
  f = \case
    SBindL i -> 'l' : show i
    SBindR i -> 'r' : show i
    SAlt   i -> 'a' : show i

-- right hoist fetch

{-
  NAMING:
    caseVar <- fetch fetchVar[0]
    itemVar <- fetch fetchVar[1] ; index > 0
    case caseVar of

  New and simplified algorithm
    - collect caseVar and fetchVar
    - move every non tagged indexed fetch under the corresponding case aletrnative and tag them accordingly
      (This is a codegen requirement.)
    - use the tag info to determine the tag arity and move only the relevant fetch operation

  RESTRICTION:
    - values come from untagged indexed fetches must not be used before the corresponding case
      otherwise it is ERROR or the operations have to be MOVED under the case along with the fetches

  HINT:
    - do not care about name aliases, that is responsibility of other transformations
    - ignore the variable liveness, the dead variable eliminiation will get rid of it
    - full traversal is necessary due to substitution ; building a var dependency graph would be more expensive
    - do not care about var dependency ; validators will reveal the faults
      (nothing should depend on values comes from non tagged fetch anyway)
-}

data Info -- analysis info
  = Info
  { fetchVars   :: Map Name Name -- caseVar -> fetchVar ; MANY - ONE
  , caseVars    :: Set Name
  }
  deriving Show

instance Monoid Info where
  mempty = Info mempty mempty
  mappend (Info a1 b1) (Info a2 b2) = Info (a1 `mappend` a2) (b1 `mappend` b2)


collectFetchVars :: Exp -> Set Name
collectFetchVars = cull . para collect where
  collect :: ExpF (Exp, Info) -> Info
  collect = \case
    EBindF (SFetchI fetchVar (Just 0), left) (Var caseVar) (_, right) -> mconcat [right, addFetchVar caseVar fetchVar]
    expInfo -> case fmap snd expInfo of
      ECaseF (Var caseVar) alts -> mconcat $ addCaseVar caseVar : alts
      e -> Data.Foldable.fold e

  addCaseVar caseVar = Info mempty (Set.singleton caseVar)
  addFetchVar caseVar fetchVar = Info (Map.singleton caseVar fetchVar) mempty

  -- keep fetch vars which have corresponding case vars
  cull Info{..} = Set.fromList . Map.elems $ Map.filterWithKey (\n _ -> Set.member n caseVars) fetchVars

{-
  FIRST VERSION:
    - no fetch var path filtering
    - move all fetches under the case, delete them when moved
    - substitute names

  TO IMPROVE:
    - do not emit dummy `pure ()` for hoised fetches, try to use futu to skip them
    - do not collect fetch variables with determined tag at analysis phase ; requres tag info / type env
      HINT: the purpose of right hoist fetch is to make all fetch operations tagged (fetch from node with known tag)
    - make it one pass:
        if it's guaranteed that the case var fetch (`caseVar <- fetch fetchVar[0]`) comes always first
        then the analysis phase can be merged with the builder phase
        this requires to distinguish between tagged and non-tagged fetches ; the former should not be touched
-}

data Build -- builder state
  = Build
  { _path       :: [Step]
  , _hoistMap   :: Map Name [(Name, Int)] -- fetchVar -> [(itemVar, fetchIndex)]
  , _caseMap    :: Map Name Name          -- caseVar  -> fetchVar
  , _emitSet    :: Set Name               -- binder names to emit ; delete prevention
  , _substMap   :: Map Name Name          -- old name -> new name
  }
  deriving Show

concat <$> mapM makeLenses [''Build]

emptyBuild = Build mempty mempty mempty mempty mempty

rightHoistFetch :: Exp -> Exp
rightHoistFetch e = {-trace (printf "fetch vars:\n%s" (ppShow globalFetchMap)) $ -}hylo skipUnit builder (emptyBuild, e)
  where
    globalFetchMap = collectFetchVars e

    builder :: (Build, Exp) -> ExpF (Build, Exp)
    builder (rhf, exp) = case exp of
      -- TODO: path tracking for bind
      -- TODO: generate according the tag arity

      EBind fetch@(SFetchI fetchVar (Just idx)) (Var name) rightExp
          | Set.member fetchVar globalFetchMap && Set.notMember name (rhf ^. emitSet) -> case idx of
              -- keep caseVar fetch and save for case recognition
              0 -> let newBuild = rhf & caseMap . at name .~ Just fetchVar
                   in EBindF (rhf, fetch) (Var $ subst (rhf^.substMap) name) (newBuild, rightExp)
              -- remove original itemVar fetch
              _ -> EBindF (rhf, SReturn Unit) Unit (rhf & hoistMap . at fetchVar . non mempty %~ ((name,idx):), rightExp)

      -- always generate bind sequences, it will be empty if there is none anyway
      ECase (Var caseVar) alts
                -- generate bind sequence for items ; clear hoist map
                -- TODO: clean up code, introduce domain specific named lenses
            -> let fetchVar = rhf ^. caseMap . at caseVar . non mempty
                   itemVars = rhf ^. hoistMap . at fetchVar . non mempty
                   newBuild = rhf & hoistMap . at fetchVar .~ Nothing
                   genAlt i alt = (altBuild, genBind fetchVar altFetch alt) where
                      altPath  = addStep (SAlt i) (newBuild ^. path)
                      pathName = showPath altPath
                      altFetch = [(printf "%s.%s" n pathName, i) | (n,i) <- itemVars]
                      altNames = map fst altFetch
                      altNameS = Set.fromList altNames
                      altSubst = Map.fromList $ zip (map fst itemVars) altNames
                      altBuild = newBuild & path .~ altPath
                                          & substMap %~ (Map.union altSubst)
                                          & emitSet %~ (mappend altNameS)
               in ECaseF (Var $ subst (rhf^.substMap) caseVar) $ zipWith genAlt [0..] alts

      _ -> (rhf,) <$> project (substVarRefExp (rhf^.substMap) exp)

    genBind :: Name -> [(Name, Int)] -> Exp -> Exp
    genBind fetchVar l (Alt cpat exp) = Alt cpat $ foldr (\(name,idx) e -> EBind (SFetchI fetchVar (Just idx)) (Var name) e) exp $ reverse l
