{-# LANGUAGE LambdaCase, RecordWildCards #-}
module PrettyLint (prettyLintExp) where

import qualified Control.Comonad.Trans.Cofree as CCTC
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Comonad.Cofree
import Data.Functor.Foldable as Foldable

import Text.PrettyPrint.ANSI.Leijen

import Grin
import Pretty
import Lint

keyword :: String -> Doc
keyword = yellow . text

keywordR = red . text

prettyLintExp :: (Cofree ExpF Int, Map Int [Error]) -> Doc
prettyLintExp (exp, errorMap) = cata folder exp where
  folder (expId CCTC.:< e) = {-fill 8 (int expId) <> -}prettyExpAlgebra e -- <> indent 2 (red $ vsep $ map text $ Map.findWithDefault [] expId errorMap) <> linebreak

  prettyExpAlgebra = \case
      ProgramF defs       -> vcat (map pretty defs)
      DefF name args exp  -> hsep (text name : map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line
      -- Exp
      EBindF simpleexp Unit exp -> pretty simpleexp <$$> pretty exp
      EBindF simpleexp lpat exp -> pretty lpat <+> text "<-" <+> pretty simpleexp <$$> pretty exp
      ECaseF val alts   -> keyword "case" <+> pretty val <+> keyword "of" <$$> indent 2 (vsep (map pretty alts))
      -- Simple Expr
      SAppF name args         -> hsep (((if isPrimName name then dullyellow else cyan) $ text name) : map pretty args)
      SReturnF val            -> keyword "pure" <+> pretty val
      SStoreF val             -> keywordR "store" <+> pretty val
      SFetchIF name Nothing   -> keywordR "fetch" <+> text name
      SFetchIF name (Just i)  -> keywordR "fetch" <+> text name <> brackets (int i)
      SUpdateF name val       -> keywordR "update" <+> text name <+> pretty val
      SBlockF exp             -> text "do" <$$> indent 2 (pretty exp)
      -- Alt
      AltF cpat exp     -> pretty cpat <+> text "->" <$$> indent 2 (pretty exp)

{-
  NOTES:
    do not use line numbers, use colors instead
    backpropagate errors to the referred expression
-}
