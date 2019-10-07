{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.ExtendedSyntax.EffectTracking.Pretty where

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vector (Vector)
import qualified Data.Vector as V

import Grin.ExtendedSyntax.Grin (Tag, Name)
import Grin.ExtendedSyntax.Pretty as Grin
import qualified AbstractInterpretation.ExtendedSyntax.EffectTracking.Result as R

instance Pretty R.Effects where
  pretty (R.Effects es) = prettyBracedList . map dullyellow . map Grin.pretty . Set.toList $ es

instance Pretty R.ETResult where
  pretty R.ETResult{..} = vsep
    [ yellow (text "Bindings")  <$$> indent 4 (prettyKeyValue $ Map.toList _register)
    , yellow (text "Functions") <$$> indent 4 (prettyKeyValue $ Map.toList _function)
    ]
