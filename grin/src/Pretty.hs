{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Pretty
  ( pretty
  , printGrin
  , PP(..)
  , prettyKeyValue
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Grin
import TypeEnv

printGrin :: Exp -> IO ()
printGrin = putDoc . pretty

-- Pretty Show instance wrapper ; i.e. useful for hspec tests
newtype PP a = PP a deriving Eq
instance Pretty a => Show (PP a ) where
  show (PP a) = show . plain . pretty $ a

keyword :: String -> Doc
keyword = yellow . text

keywordR = red . text

-- TODO
--  nice colors for syntax highlight
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
      SAppF name args         -> hsep (((if isPrimName name then dullyellow else cyan) $ text name) : map pretty args)
      SReturnF val            -> keyword "pure" <+> pretty val
      SStoreF val             -> keywordR "store" <+> pretty val
      SFetchIF name Nothing   -> keywordR "fetch" <+> text name
      SFetchIF name (Just i)  -> keywordR "fetch" <+> text name <> brackets (int i)
      SUpdateF name val       -> keywordR "update" <+> text name <+> pretty val
      SBlockF exp             -> text "do" <$$> indent 2 (pretty exp)
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
  pretty = \case
    LInt64 a  -> integer $ fromIntegral a
    LWord64 a -> integer (fromIntegral a) <> text "u"
    LFloat a  -> float a
    LBool a   -> text "#" <> text (show a)

instance Pretty CPat where
  pretty = \case
    NodePat tag vars  -> parens $ hsep (pretty tag : map text vars)
    TagPat  tag       -> pretty tag
    LitPat  lit       -> pretty lit

instance Pretty TagType where
  pretty = green . \case
    C -> text "C"
    F -> text "F"
    P -> text "P"

instance Pretty Tag where
  pretty (Tag tagtype name) = pretty tagtype <> text name

-- generic ; used by HPTResult and TypeEnv

instance Pretty a => Pretty (Set a) where
  pretty s = encloseSep lbrace rbrace comma (map pretty $ Set.toList s)

prettyKeyValue :: (Pretty k, Pretty v) => [(k,v)] -> Doc
prettyKeyValue kvList = vsep [fill 6 (pretty k) <+> text "->" <+> pretty v | (k,v) <- kvList]

-- type env

instance Pretty SimpleType where
  pretty = \case
    T_Location l  -> encloseSep lbrace rbrace comma $ map (cyan . int . succ) l
    ty            -> red $ text $ show ty

prettyNode :: (Tag, Vector SimpleType) -> Doc
prettyNode (tag, args) = pretty tag <> list (map pretty $ V.toList args)

prettyFunction :: (Name, (Type, Vector Type)) -> Doc
prettyFunction (name, (ret, args)) = text name <> align (encloseSep (text " :: ") empty (text " -> ") (map pretty $ (V.toList args) ++ [ret]))

instance Pretty Type where
  pretty = \case
    T_SimpleType ty -> pretty ty
    T_NodeSet ns    -> encloseSep lbrace rbrace comma (map prettyNode (Map.toList ns))

instance Pretty TypeEnv where
  pretty TypeEnv{..} = vsep
    [ yellow (text "Location") <$$> indent 4 (prettyKeyValue $ zip [(1 :: Int)..] $ map T_NodeSet $ V.toList _location)
    , yellow (text "Variable") <$$> indent 4 (prettyKeyValue $ Map.toList _variable)
    , yellow (text "Function") <$$> indent 4 (vsep $ map prettyFunction $ Map.toList _function)
    ]

