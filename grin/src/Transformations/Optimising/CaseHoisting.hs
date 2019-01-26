{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.CaseHoisting where

import Control.Monad
import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Bifunctor (first)

import Grin.Grin
import Grin.TypeEnv
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
      | Just (T_NodeSet ns) <- mTypeOfValTE typeEnv val
      -> Just (Map.keysSet ns)

    SAppF name _
      | T_NodeSet ns <- fst $ functionType typeEnv name
      -> Just (Map.keysSet ns)

    SFetchIF name Nothing
      | T_SimpleType (T_Location locs) <- variableType typeEnv name
      -> Just (mconcat [Map.keysSet (_location typeEnv Vector.! loc) | loc <- locs])

    _ -> Nothing


caseHoisting :: TypeEnv -> Exp -> (Exp, ExpChanges)
caseHoisting typeEnv exp = first fst $ evalNameM exp $ histoM folder exp where

  folder :: ExpF (Cofree ExpF (Exp, Set Name)) -> NameM (Exp, Set Name)
  folder exp = case exp of
    -- middle case
    EBindF ((ECase val alts1, leftUse) :< _)  (Var lpatName)
      (_ :< (EBindF ((ECase (Var varName) alts2, caseUse) :< _) lpat ((rightExp, rightUse) :< _)))
        | lpatName == varName
        , Just alts1Types <- sequence $ map (getReturnTagSet typeEnv) alts1
        , Just matchList <- disjointMatch (zip alts1Types alts1) alts2
        , Set.notMember varName rightUse -- allow only linear variables ; that are not used later
        -> do
          hoistedAlts <- mapM (hoistAlts lpatName) matchList
          pure (EBind (ECase val hoistedAlts) lpat rightExp, Set.delete varName $ mconcat [leftUse, caseUse, rightUse])

    -- last case
    EBindF ((ECase val alts1, leftUse) :< _) (Var lpatName) ((ECase (Var varName) alts2, rightUse) :< _)
        | lpatName == varName
        , Just alts1Types <- sequence $ map (getReturnTagSet typeEnv) alts1
        , Just matchList <- disjointMatch (zip alts1Types alts1) alts2
        -> do
          hoistedAlts <- mapM (hoistAlts lpatName) matchList
          pure (ECase val hoistedAlts, Set.delete varName $ mconcat [leftUse, rightUse])

    _ -> let useSub = Data.Foldable.fold (snd . extract <$> exp)
             useExp = foldNameUseExpF Set.singleton exp
         in pure (embed (fst . extract <$> exp), mconcat [useSub, useExp])

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
    done - ignore non linear scrutinee
      IDEA:
        this could be supported if product type was available in GRIN then the second case could return from the hoisted case with a pair of the original two case results
-}
