{-# LANGUAGE LambdaCase #-}
module Pretty where

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Grin

printGrin :: Exp -> IO ()
printGrin = putDoc . pretty

keyword :: String -> Doc
keyword = yellow . text

keywordR = red . text

-- TODO
--  nice colors for syntax highlight
--  precedence support
--  better node type syntax (C | F | P)

instance Pretty Exp where
  pretty = cata folder where
    folder = \case
      ProgramF defs       -> vcat (map pretty defs)
      DefF name args exp  -> hsep (text name : map pretty args) <+> text "=" <$$> indent 2 (pretty exp) <> line
      -- Exp
      EBindF simpleexp Unit exp -> pretty simpleexp <$$> pretty exp
      EBindF simpleexp lpat exp -> pretty lpat <+> text "<-" <+> pretty simpleexp <$$> pretty exp
      ECaseF val alts   -> keyword "case" <+> pretty val <+> keyword "of" <$$> indent 2 (vsep (map pretty alts))
      -- Simple Expr
      SAppF name args   -> hsep (text name : map pretty args)
      SReturnF val      -> text "return" <+> pretty val
      SStoreF val       -> keywordR "store" <+> pretty val
      SFetchF name      -> keywordR "fetch" <+> text name
      SUpdateF name val -> keywordR "update" <+> text name <+> pretty val
      SBlockF exp       -> text "do" <$$> indent 2 (pretty exp)
      -- Alt
      AltF cpat exp     -> pretty cpat <+> text "->" <+> align (pretty exp)

instance Pretty Val where
  pretty = \case
    ConstTagNode tag args -> parens $ hsep (pretty tag : map pretty args)
    VarTagNode name args  -> parens $ hsep (text name : map pretty args)
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
    NodePat tag vars  -> hsep (pretty tag : map text vars)
    TagPat  tag       -> pretty tag
    LitPat  lit       -> pretty lit

instance Pretty TagType where
  pretty = green . \case
    C -> text "C"
    F -> text "F"
    P -> text "P"

instance Pretty Tag where
  pretty (Tag tagtype name _) = pretty tagtype <> text name
