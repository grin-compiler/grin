{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Transformations.EffectMap
  ( effectMap
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Lens.Micro
import Data.List
import Data.Maybe
import Debug.Trace

import Grin.Grin
import Grin.TypeEnv
import Grin.EffectMap
import Transformations.Util


effectMap :: (TypeEnv, Exp) -> EffectMap
effectMap (te, e) = EffectMap $ withEffectfulExternals $ effectfulFunctions $ unMMap $ snd $ para buildEffectMap e where

  withEffectfulExternals :: Map Name Effects -> Map Name Effects
  withEffectfulExternals
    | Program exts _ <- e
    = Map.union $ Map.fromSet (\ext -> Effects (Set.singleton ext) mempty mempty) effectfulExternals
    | otherwise = id

  effectfulExternals :: Set Name
  effectfulExternals = case e of
    Program es _ -> Set.fromList $ map eName $ filter eEffectful es
    _            -> Set.empty

  buildEffectMap :: ExpF (Exp, (Set EffectWithCalls, MonoidMap Name (Set EffectWithCalls))) -> (Set EffectWithCalls, MonoidMap Name (Set EffectWithCalls))
  buildEffectMap =  \case
    DefF name _ (_,(effs, _)) -> (mempty, MMap $ Map.singleton name effs)
    EBindF (SStore _,lhs) (Var v) (_,rhs)
      | Just locs <- te ^? variable . at v . _Just . _T_SimpleType . _T_Location
      -> let storeEff = (Set.singleton $ Effect $ storesEff locs, mempty)
         in lhs <> rhs <> storeEff
    SAppF name _
      | Set.member name effectfulExternals -> (Set.singleton (Effect $ primopEff name), mempty)
      | otherwise -> (Set.singleton (Call name), mempty)
    SUpdateF name _
      | Just locs <- te ^? variable . at name . _Just . _T_SimpleType . _T_Location
      -> (Set.singleton $ Effect $ updatesEff locs, mempty)
    rest -> Data.Foldable.fold . fmap snd $ rest



data EffectWithCalls
  = Effect { toEffects     :: Effects }
  | Call   { callsFunction :: Name }
  deriving (Eq, Show, Ord)

-- Removes the calls information and collects all the transitive effects.
-- Returns a Map that contains only the effectful function calls.
effectfulFunctions :: Map Name (Set EffectWithCalls) -> Map Name Effects
effectfulFunctions em = removeCalls $ go em where

  removeCalls = Map.map (foldMap toEffects) . Map.filter (not . Set.null) . Map.map (Set.filter (not . isCall))

  go em0 = let em1 = flip Map.map em0 $ \es ->
                        let (calls, rest) = Set.partition isCall es
                        in Set.unions [calls, rest, mconcat $ map (fromMaybe mempty . flip Map.lookup em . callsFunction) $ Set.toList calls]
           in if em0 == em1 then em0 else go em1

  isCall = \case
    Call _ -> True
    _      -> False

-- MonoidMap

newtype MonoidMap k m = MMap { unMMap :: Map k m }
  deriving Show

instance (Ord k, Semigroup m) => Semigroup (MonoidMap k m) where
  (MMap m1) <> (MMap m2) = MMap (Map.unionWith (<>) m1 m2)
instance (Ord k, Monoid m) => Monoid (MonoidMap k m) where
  mempty = MMap mempty
  mappend (MMap m1) (MMap m2) = MMap (Map.unionWith mappend m1 m2)
