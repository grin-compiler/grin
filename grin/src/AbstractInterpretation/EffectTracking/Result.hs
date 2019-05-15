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

import Grin.Grin (Name, Tag)
import AbstractInterpretation.IR hiding (Tag, SimpleType)
import AbstractInterpretation.Reduce (ComputerState(..))
import qualified Grin.TypeEnv as TypeEnv
import qualified AbstractInterpretation.IR as IR
import qualified AbstractInterpretation.Reduce as R
import AbstractInterpretation.EffectTracking.CodeGenBase (ETMapping(..))

newtype Effects = Effects { _effectSet :: Set Name }
  deriving (Eq, Ord, Show)

data ETResult = ETResult
  { _register :: Map Name Effects
  , _function :: Map Name Effects
  } deriving (Eq, Ord, Show)

toETResult :: ETMapping -> R.ComputerState -> ETResult
toETResult e@ETMapping{..} c@R.ComputerState{..} = ETResult
  { _register = Map.map (convertReg e c) _etRegisterMap
  , _function = Map.map (convertReg e c) _etFunctionRetMap
  }

convertReg :: ETMapping -> ComputerState -> Reg -> Effects
convertReg etMap R.ComputerState{..} (Reg i) = convertValue etMap $ _register V.! (fromIntegral i)

-- TODO:
convertValue :: ETMapping -> R.Value -> Effects
convertValue ETMapping{..} (R.Value ty _) = Effects $ Set.map (fromJust . flip Map.lookup _etExternalMap) ty
