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
          hoistedAlts <- forM matchList $ \(Alt cpat1 alt1, Alt cpat2 alt2) -> do
            freshLPatName <- deriveNewName lpatName
            let nameMap = Map.singleton lpatName freshLPatName
            (freshAlt2, _) <- case cpat2 of
              DefaultPat  -> refreshNames nameMap alt2
              _           -> refreshNames nameMap $ EBind (SReturn $ Var freshLPatName) (cpatToLPat cpat2) alt2
            pure . Alt cpat1 $ EBind (SBlock alt1) (Var freshLPatName) freshAlt2

          pure $ EBindF (ECase val hoistedAlts) lpat rightExp

    -- TODO: factor out code duplication

    -- last case
    EBind (ECase val alts1) (Var lpatName) (ECase (Var varName) alts2)
        | lpatName == varName
        , Just alts1Types <- sequence $ map (getReturnTagSet typeEnv) alts1
        , Just matchList <- disjointMatch (zip alts1Types alts1) alts2
        -> do
          hoistedAlts <- forM matchList $ \(Alt cpat1 alt1, Alt cpat2 alt2) -> do
            freshLPatName <- deriveNewName lpatName
            let nameMap = Map.singleton lpatName freshLPatName
            (freshAlt2, _) <- case cpat2 of
              DefaultPat  -> refreshNames nameMap alt2
              _           -> refreshNames nameMap $ EBind (SReturn $ Var freshLPatName) (cpatToLPat cpat2) alt2
            pure . Alt cpat1 $ EBind (SBlock alt1) (Var freshLPatName) freshAlt2

          pure $ ECaseF val hoistedAlts

    _ -> pure (project exp)

disjointMatch :: [(Set Tag, Alt)] -> [Alt] -> Maybe [(Alt, Alt)]
disjointMatch tsAlts1 alts2
  | and [Set.size ts == 1 | (ts, _) <- tsAlts1]
  , length tsAlts1 == Set.size (mconcat . map fst $ tsAlts1)
  = Just
      [ (alt1, alt2)
      | (ts, alt1) <- tsAlts1
      , let tag = Set.findMin ts
      , alt2@(Alt (NodePat patTag _) _) <- alts2
      , patTag == tag
      ]
disjointMatch _ _ = Nothing

{-
  TODO:
    - default pat
    - check non overlapping coverage
-}
