{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, DeriveFunctor, ViewPatterns #-}
module AbstractInterpretation.EffectTracking.Result where

import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap

import Data.Maybe

import Lens.Micro.Platform
import Lens.Micro.Extra

import Grin.Grin (Name, Tag, External(..))
import Grin.Pretty
import AbstractInterpretation.IR hiding (Tag, SimpleType)
import AbstractInterpretation.Reduce (ComputerState(..))
import qualified Grin.TypeEnv as TypeEnv
import qualified AbstractInterpretation.IR as IR
import qualified AbstractInterpretation.Reduce as R
import AbstractInterpretation.EffectTracking.CodeGenBase (ETMapping(..))


newtype Effects = Effects { _effectSet :: Set Name }
  deriving (Eq, Ord, Show, Semigroup, Monoid)

data ETResult = ETResult
  { _register :: Map Name Effects
  , _function :: Map Name Effects
  , _external :: Map Name External
  } deriving (Eq, Ord, Show)

instance Semigroup ETResult where
  (<>) (ETResult reg1 fun1 ext1) (ETResult reg2 fun2 ext2)
    = ETResult (reg1 <> reg2) (fun1 <> fun2) (ext1 <> ext2)

instance Monoid ETResult where
  mempty = ETResult mempty mempty mempty

hasSideEffectVar :: ETResult -> Name -> Bool
hasSideEffectVar ETResult{..} v
  | Just (Effects effs) <- Map.lookup v _register
  = not . null $ effs
  | otherwise = error $ "Variable " ++ show (PP v) ++ " is not present in the effect analysis result"

hasSideEffectFun :: ETResult -> Name -> Bool
hasSideEffectFun ETResult{..} f
  | Just ext <- Map.lookup f _external
  = eEffectful ext
  | Just (Effects effs) <- Map.lookup f _function
  = not . null $ effs
  | otherwise = error $ "Function " ++ show (PP f) ++ " is not present in the effect analysis result"

hasSideEffect :: ETResult -> Name -> Bool
hasSideEffect ETResult{..} name
  | Just (Effects effs) <- Map.lookup name _register
  = not . null $ effs
  | Just ext <- Map.lookup name _external
  = eEffectful ext
  | Just (Effects effs) <- Map.lookup name _function
  = not . null $ effs
  | otherwise = error $ "Entity " ++ show (PP name) ++ " is not present in the effect analysis result"

toETResult :: ETMapping -> R.ComputerState -> ETResult
toETResult e@ETMapping{..} c@R.ComputerState{..} = ETResult
  { _register = Map.map (convertReg e c) _etRegisterMap
  , _function = Map.map (convertReg e c) _etFunctionRetMap
  , _external = Map.fromList . map toMapEntry . Map.elems $ _etExternalMap
  } where

    convertReg :: ETMapping -> ComputerState -> Reg -> Effects
    convertReg etMap R.ComputerState{..} (Reg i) = convertValue etMap $ _register V.! (fromIntegral i)

    convertValue :: ETMapping -> R.Value -> Effects
    convertValue ETMapping{..} (R.Value ty _) = Effects $ Set.map (eName . fromJust . flip Map.lookup _etExternalMap) ty

    toMapEntry :: External -> (Name, External)
    toMapEntry = (,) <$> eName <*> id
