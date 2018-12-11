{-# LANGUAGE RecordWildCards #-}
module AbstractInterpretation.LiveVariable.Pretty where

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as V

import Grin.Pretty
import Grin.Grin (Tag, Name)

import AbstractInterpretation.LiveVariable.Result
import AbstractInterpretation.HeapPointsTo.Pretty

prettyLiveness :: Bool -> Doc
prettyLiveness True  = green (text "LIVE")
prettyLiveness False = red   (text "DEAD")

instance Pretty Node where
  pretty (Node t fs) = pTag <> pFields where
    pTag    = parens . prettyLiveness $ t
    pFields = list . map prettyLiveness . V.toList $ fs

instance Pretty Liveness where
  pretty (BasicVal l) = prettyLiveness l
  pretty (NodeSet ns) = prettyBracedList
                      . map prettySimplePair
                      . Map.toList $ ns

instance Pretty LVAResult where
  pretty LVAResult{..} = vsep
    [ yellow (text "Heap") <$$> indent 4 (prettyKeyValue $ zip [(0 :: Int)..] $ V.toList _memory)
    , yellow (text "Env") <$$> indent 4 (prettyKeyValue $ Map.toList  _register)
    , yellow (text "Function") <$$> indent 4 (vsep $ map prettyFunction $ Map.toList _function)
    ]
