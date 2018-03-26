{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Transformations.Optimising.GeneralizedUnboxing where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import Data.Function
import Data.Functor.Foldable as Foldable
import Grin
import TypeEnv
import Pretty
import Control.Applicative
import Lens.Micro.Platform
import Debug.Trace


generalizedUnboxing :: (TypeEnv, Exp) -> (TypeEnv, Exp)
generalizedUnboxing = id

{-
Step 1: Find function to unbox
Step 2: Transform function returns
Step 3: Transform function calls
-}

tailCalls :: Exp -> Maybe [Name]
tailCalls = cata collect where
  collect :: ExpF (Maybe [Name]) -> Maybe [Name]
  collect = \case
    DefF _ _ result   -> result
    EBindF _ _ result -> result
    ECaseF _ alts
      | all isJust alts -> nub <$> mconcat alts
      | otherwise       -> Nothing
    AltF _ result -> result
    SAppF f _     -> Just [f]
    e -> Nothing

doesReturnAKnownProduct :: TypeEnv -> Name -> Bool
doesReturnAKnownProduct te name =
  te ^? function . at name . _Just . _1 . _T_NodeSet . to Map.size
      & maybe False (==1)

functionsToUnbox :: (TypeEnv, Exp) -> [Name]
functionsToUnbox (te, Program defs) = result where
  funName (Def n _ _) = n

  tailCallsMap :: Map Name [Name]
  tailCallsMap = Map.fromList $ catMaybes $ map (\e -> (,) (funName e) <$> tailCalls e) $ defs

  result = step initial
  initial = map funName $ filter (doesReturnAKnownProduct te . funName) defs
  disqualify candidates = filter
    (\candidate -> case Map.lookup candidate tailCallsMap of
      Nothing    -> True
      Just calls -> all (`elem` candidates) calls)
    candidates
  step = fix $ \rec x0 ->
    let x1 = disqualify x0 in
    if x0 == x1
      then x0
      else rec x1
