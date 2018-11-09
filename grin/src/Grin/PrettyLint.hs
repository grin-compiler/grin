{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Grin.PrettyLint (prettyLintExp) where

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
prettyLintExp (exp, errorMap) = cata folder exp where
  folder (expId CCTC.:< e) = addError expId (prettyExpAlgebra e)

  addError expId d =
    maybe
      d
      ((d <>) . red . (string " <- "<>) . align . vsep . map string )
      (Map.lookup expId errorMap)

  prettyExpAlgebra = \case
      ProgramF defs       -> vcat (map pretty defs)
      DefF name args exp  -> hsep (pretty name : map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line
      -- Exp
      EBindF simpleexp Unit exp -> pretty simpleexp <$$> pretty exp
      EBindF simpleexp lpat exp -> pretty lpat <+> text "<-" <+> pretty simpleexp <$$> pretty exp
      ECaseF val alts   -> keyword "case" <+> pretty val <+> keyword "of" <$$> indent 2 (vsep (map pretty alts))
      -- Simple Expr
      SAppF name args         -> hsep (((if isPrimName name then dullyellow else cyan) $ pretty name) : map pretty args)
      SReturnF val            -> keyword "pure" <+> pretty val
      SStoreF val             -> keywordR "store" <+> pretty val
      SFetchIF name Nothing   -> keywordR "fetch" <+> pretty name
      SFetchIF name (Just i)  -> keywordR "fetch" <+> pretty name <> brackets (int i)
      SUpdateF name val       -> keywordR "update" <+> pretty name <+> pretty val
      SBlockF exp             -> text "do" <$$> indent 2 (pretty exp)
      -- Alt
      AltF cpat exp     -> pretty cpat <+> text "->" <$$> indent 2 (pretty exp)

{-
  NOTES:
    do not use line numbers, use colors instead
    backpropagate errors to the referred expression
-}
