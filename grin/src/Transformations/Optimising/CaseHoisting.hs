{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.CaseHoisting where

import Control.Monad
import Control.Comonad.Cofree
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Grin
import TypeEnv
import Transformations.Util
import Transformations.Names

{-
  IDEA:
    If Alt had name then the HPT could calculate it's return type and store in TypeEnv
-}

getReturnTagSet :: TypeEnv -> Exp -> Maybe (Set Tag)
getReturnTagSet typeEnv = cata folder where
  folder exp = case exp of
    EBindF _ _ ts -> ts
    SBlockF ts    -> ts
    AltF _ ts     -> ts
    ECaseF _ alts -> mconcat <$> sequence alts

    SReturnF val
      | T_NodeSet ns <- typeOfValTE typeEnv val
      -> Just (Map.keysSet ns)

    SAppF name _
      | T_NodeSet ns <- fst $ functionType typeEnv name
      -> Just (Map.keysSet ns)

    SFetchIF name Nothing
      | T_SimpleType (T_Location locs) <- variableType typeEnv name
      -> Just (mconcat [Map.keysSet (_location typeEnv Vector.! loc) | loc <- locs])

    _ -> Nothing


caseHoisting :: (TypeEnv, Exp) -> (TypeEnv, Exp)
caseHoisting (typeEnv, exp) = (typeEnv, evalNameM $ anaM builder exp) where

  builder :: Exp -> NameM (ExpF Exp)
  builder exp = case exp of
    -- middle case
    EBind    (ECase val           alts1)  (Var lpatName)
      (EBind (ECase (Var varName) alts2)  lpat            rightExp)
        | lpatName == varName
        , Just alts1Types <- sequence $ map (getReturnTagSet typeEnv) alts1
        , Just matchList <- disjointMatch (zip alts1Types alts1) alts2
        -> do
          hoistedAlts <- mapM (hoistAlts lpatName) matchList
          pure $ EBindF (ECase val hoistedAlts) lpat rightExp

    -- last case
    EBind (ECase val alts1) (Var lpatName) (ECase (Var varName) alts2)
        | lpatName == varName
        , Just alts1Types <- sequence $ map (getReturnTagSet typeEnv) alts1
        , Just matchList <- disjointMatch (zip alts1Types alts1) alts2
        -> ECaseF val <$> mapM (hoistAlts lpatName) matchList

    _ -> pure (project exp)

hoistAlts :: Name -> (Alt, Alt) -> NameM Alt
hoistAlts lpatName (Alt cpat1 alt1, Alt cpat2 alt2) = do
  freshLPatName <- deriveNewName lpatName
  let nameMap = Map.singleton lpatName freshLPatName
  (freshAlt2, _) <- case cpat2 of
    DefaultPat  -> refreshNames nameMap alt2
    _           -> refreshNames nameMap $ EBind (SReturn $ Var freshLPatName) (cpatToLPat cpat2) alt2
  pure . Alt cpat1 $ EBind (SBlock alt1) (Var freshLPatName) freshAlt2

disjointMatch :: [(Set Tag, Alt)] -> [Alt] -> Maybe [(Alt, Alt)]
disjointMatch tsAlts1 alts2
  | Just (defaults, tagMap) <- mconcat <$> mapM groupByCPats alts2
  , length defaults <= 1
  , Just (altPairs, _, _) <- Data.Foldable.foldrM (matchAlt tagMap) ([], defaults, Set.empty) tsAlts1
  = Just altPairs
disjointMatch _ _ = Nothing

groupByCPats :: Alt -> Maybe ([Alt], Map Tag Alt)
groupByCPats alt@(Alt cpat _) = case cpat of
  DefaultPat    -> Just ([alt], mempty)
  NodePat tag _ -> Just ([], Map.singleton tag alt)
  _             -> Nothing

matchAlt :: Map Tag Alt -> (Set Tag, Alt) -> ([(Alt, Alt)], [Alt], Set Tag) -> Maybe ([(Alt, Alt)], [Alt], Set Tag)
matchAlt tagMap (ts, alt1) (matchList, defaults, coveredTags)
  -- regular node pattern
  | Set.size ts == 1
  , tag <- Set.findMin ts
  , Set.notMember tag coveredTags
  , Just alt2 <- Map.lookup tag tagMap
  = Just ((alt1, alt2):matchList, defaults, Set.insert tag coveredTags)

  -- default can handle this
  | defaultAlt:[] <- defaults
  , Data.Foldable.all (flip Set.notMember coveredTags) ts
  = Just ((alt1, defaultAlt):matchList, [], coveredTags `mappend` ts)

  | otherwise = Nothing

{-
  TODO:
    - add cloned variables to TypeEnv
-}
