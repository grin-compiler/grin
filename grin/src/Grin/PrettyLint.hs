{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Grin.PrettyLint (prettyLintExp, prettyAnnExp) where

import Prelude hiding ((<$>))
import qualified Control.Comonad.Trans.Cofree as CCTC
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Comonad.Cofree
import Data.Functor.Foldable as Foldable

import Text.PrettyPrint.ANSI.Leijen

import Grin.Grin
import Grin.Pretty
import Grin.Lint

keyword :: String -> Doc
keyword = yellow . text
keywordR = red . text

prettyLintExp :: (Cofree ExpF Int, Map Int [Error]) -> Doc
prettyLintExp (cexp, errorMap) = prettyAnnExp $ fmap addError cexp where
  addError expId d =
    maybe
      d
      (\errors ->
        if (any before errors)
          then (<$> d) . red . align . vsep . map (string . ("-- LINT: " <>) . message) $ errors
          else (d <>) . red . (string " -- LINT: "<>) . align . vsep . map (string . message) $ errors)
      (Map.lookup expId errorMap)

prettyAnnExp :: Cofree ExpF (Doc -> Doc) -> Doc
prettyAnnExp exp = cata folder exp where
  exts = case exp of
    (_ :< ProgramF es _) -> es
    _                    -> []

  folder (ann CCTC.:< e) = ann (prettyExpAlgebra e)
