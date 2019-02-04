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

  prettyExpAlgebra = \case
      ProgramF exts defs  -> vcat (map pretty defs)
      DefF name args exp  -> hsep (pretty name : map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line
      -- Exp
      EBindF simpleexp Unit exp -> pretty simpleexp <$$> pretty exp
      EBindF simpleexp lpat exp -> pretty lpat <+> text "<-" <+> pretty simpleexp <$$> pretty exp
      ECaseF val alts   -> keyword "case" <+> pretty val <+> keyword "of" <$$> indent 2 (vsep (map pretty alts))
      -- Simple Expr
      SAppF name args         -> hsep (((if isExternalName exts name then dullyellow else cyan) $ pretty name) : map pretty args)
      SReturnF val            -> keyword "pure" <+> pretty val
      SStoreF val             -> keywordR "store" <+> pretty val
      SFetchIF name Nothing   -> keywordR "fetch" <+> pretty name
      SFetchIF name (Just i)  -> keywordR "fetch" <+> pretty name <> brackets (int i)
      SUpdateF name val       -> keywordR "update" <+> pretty name <+> pretty val
      SBlockF exp             -> text "do" <$$> indent 2 (pretty exp)
      -- Alt
      AltF cpat exp     -> pretty cpat <+> text "->" <$$> indent 2 (pretty exp)
