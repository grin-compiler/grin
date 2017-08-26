{-# LANGUAGE LambdaCase #-}
module Pretty where

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Grin

printGrin :: [Def] -> IO ()
printGrin defs = putDoc $ vcat (map pretty defs)

keyword :: String -> Doc
keyword = yellow . text

instance Pretty Def where
  pretty (Def name args exp) = text name <+> hsep (map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line

instance Pretty Exp where
  pretty = cata folder where
    folder = \case
      -- Exp
      EBindF simpleexp lpat exp -> pretty lpat <+> text "<-" <+> pretty simpleexp <$$> pretty exp
      ECaseF val alts   -> keyword "case" <+> pretty val <+> keyword "of" <$$> indent 2 (vsep (map pretty alts))
      -- Simple Expr
      SAppF name args   -> text name <+> hsep (map pretty args)
      SReturnF val      -> keyword "return" <+> pretty val
      SStoreF val       -> keyword "store" <+> pretty val
      SFetchF name      -> keyword "fetch" <+> text name
      SUpdateF name val -> keyword "update" <+> text name <+> pretty val
      SBlockF exp       -> keyword "do" <$$> indent 2 (pretty exp)
      -- Alt
      AltF cpat exp     -> pretty cpat <+> text "->" <$$> indent 2 (pretty exp)

instance Pretty Val where
  pretty = \case
    ConstTagNode tag args -> pretty tag <+> hsep (map pretty args)
    VarTagNode name args  -> text name <+> hsep (map pretty args)
    ValTag tag  -> pretty tag
    Unit        -> parens empty
    -- simple val
    Lit lit     -> pretty lit
    Var name    -> text name
    -- extra
    Loc a       -> keyword "loc" <+> int a
    Undefined   -> keyword "undefined"

instance Pretty Lit where
  pretty (LFloat a) = float a

instance Pretty CPat where
  pretty = \case
    NodePat tag vars  -> pretty tag <+> hsep (map text vars)
    TagPat  tag       -> pretty tag
    LitPat  lit       -> pretty lit

instance Pretty TagType where
  pretty = \case
    C -> keyword "C"
    F -> keyword "F"
    P -> keyword "P"

instance Pretty Tag where
  pretty (Tag tagtype name _) = pretty tagtype <> text name
