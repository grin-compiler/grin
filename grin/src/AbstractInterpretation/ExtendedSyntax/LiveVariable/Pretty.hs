{-# LANGUAGE RecordWildCards #-}
module AbstractInterpretation.ExtendedSyntax.LiveVariable.Pretty where

import Data.Tuple
import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector (Vector)
import qualified Data.Vector as V

import Lens.Micro.Platform

import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.Grin (Tag, Name)

import AbstractInterpretation.ExtendedSyntax.LiveVariable.Result
--import AbstractInterpretation.HeapPointsTo.Pretty

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
  pretty = prettyLVAResult

prettyLVAResult :: LVAResult -> Doc
prettyLVAResult LVAResult{..} = vsep
  [ yellow (text "Heap") <$$> indent 4 (prettyKeyValue $ zip [(0 :: Int)..] $ V.toList _memory)
  , yellow (text "Env (* is effectful)") <$$> indent 4 (prettyKeyValue . mapFst annotateEffectfulName . Map.toList $ _registerLv)
  , yellow (text "Function (* is effectful)") <$$> indent 4 (vsep . map prettyFunction . mapFst annotateEffectfulName . Map.toList $ _functionLv)
  ] where
    annotateEffectfulName name
      | Just (Effect True) <- Map.lookup name _registerEff
      = text "*" <> pretty name
      | Just (Effect True) <- Map.lookup name _functionEff
      = text "*" <> pretty name
      | otherwise = pretty name

    mapFst f = map (over _1 f)

