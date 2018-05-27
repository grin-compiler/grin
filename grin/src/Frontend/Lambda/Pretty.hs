{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Frontend.Lambda.Pretty
  ( printLambda
  ) where

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Frontend.Lambda.Syntax
import Grin (isPrimName)

printLambda :: Exp -> IO ()
printLambda = putDoc . pretty


keyword :: String -> Doc
keyword = yellow . text

keywordR = red . text

prettyBind (name, exp) = text name <+> text "=" <+> pretty exp

instance Pretty Exp where
  pretty = cata folder where
    folder = \case
      ProgramF defs       -> vcat (map pretty defs)
      DefF name args exp  -> hsep (text name : map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line
      -- Exp
      AppF name args      -> hsep (((if isPrimName name then dullyellow else cyan) $ text name) : map pretty args)
      CaseF atom alts     -> keyword "case" <+> pretty atom <+> keyword "of" <$$> indent 2 (vsep (map pretty alts))
      LetF binds exp      -> keyword "let"    <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      LetRecF binds exp   -> keyword "letrec" <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      LetSF binds exp     -> keyword "letS"   <+> align (vsep (map prettyBind binds)) <$$> pretty exp
      ConF tag args       -> parens $ hsep (text tag : map pretty args)
      -- Atom
      VarF name           -> text name
      LitF lit            -> pretty lit
      -- Alt
      AltF cpat exp       -> pretty cpat <+> text "->" <+> align (pretty exp)

instance Pretty Lit where
  pretty = \case
    LInt64 a  -> integer $ fromIntegral a
    LWord64 a -> integer (fromIntegral a) <> text "u"
    LFloat a  -> float a
    LBool a   -> text "#" <> text (show a)

instance Pretty Pat where
  pretty = \case
    NodePat tag vars  -> parens $ hsep (text tag : map text vars)
    LitPat  lit       -> pretty lit
    DefaultPat        -> keyword "_"
