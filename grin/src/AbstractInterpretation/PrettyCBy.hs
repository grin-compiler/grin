{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.PrettyCBy where

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Data.Map (Map)
import qualified Data.Map as Map

import Grin.Pretty
import Grin.Grin (Tag, Name)

import AbstractInterpretation.CByResult
import AbstractInterpretation.PrettyHPT

instance Pretty ProducerSet where
  pretty (ProducerSet ps) = prettyBracedList
                          . map prettySimplePair
                          . Map.toList $ ps

instance Pretty ProducerMap where
  pretty (ProducerMap pm) = prettyKeyValue $ Map.toList pm

instance Pretty CByResult where
  pretty CByResult{..} = vsep
    [ pretty _hptResult
    , yellow (text "Producers") <$$> indent 4 (pretty _producers)
    ]
