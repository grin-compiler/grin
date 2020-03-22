{-# LANGUAGE RecordWildCards #-}
module AbstractInterpretation.ExtendedSyntax.CreatedBy.Pretty where

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Data.Map (Map)
import qualified Data.Map as Map

import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.Grin (Tag, Name)

import AbstractInterpretation.ExtendedSyntax.CreatedBy.Result
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Pretty

instance Pretty ProducerSet where
  pretty (ProducerSet ps) = prettyBracedList
                          . map prettySimplePair
                          . Map.toList $ ps

instance Pretty ProducerMap where
  pretty (ProducerMap pm) = prettyKeyValue $ Map.toList pm

instance Pretty ProducerGraph where
  pretty (ProducerGraph pMap) = pretty pMap

instance Pretty GroupedProducers where
  pretty (All    prods) = yellow (text "Producer Groups (all)")
                          <$$> indent 4 (pretty prods)
  pretty (Active prods) = yellow (text "Producer Groups (only for actives)")
                          <$$> indent 4 (pretty prods)

instance Pretty CByResult where
  pretty CByResult{..} = vsep
    [ pretty _hptResult
    , yellow (text "Producers") <$$> indent 4 (pretty _producers)
    , pretty _groupedProducers
    ]
