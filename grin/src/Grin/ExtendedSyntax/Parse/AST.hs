{-# LANGUAGE TupleSections, LambdaCase, OverloadedStrings #-}

module Grin.ExtendedSyntax.Parse.AST
  ( parseGrin
  , parseProg
  , parseDef
  , parseExpr
  ) where

import Data.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative (empty)
import Control.Monad (void, mzero)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char as C
import qualified Data.Set as Set

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Parse.Basic
import Grin.ExtendedSyntax.Parse.TypeEnv

-- grin syntax

def :: Parser Def
def = Def <$> try (L.indentGuard sc EQ pos1 *> var) <*> many var <* op "=" <*> (L.indentGuard sc GT pos1 >>= expr)

expr :: Pos -> Parser Exp
expr i = L.indentGuard sc EQ i >>
  try ((\pat e b -> EBind e pat b) <$> try (bindingPat <* op "<-") <*> simpleExp i <*> expr i ) <|>
  simpleExp i

simpleExp :: Pos -> Parser SimpleExp
simpleExp i = SReturn <$ kw "pure" <*> value <|>
              ECase <$ kw "case" <*> var <* kw "of" <*> (L.indentGuard sc GT i >>= some . alternative) <|>
              SStore <$ kw "store" <*> var <|>
              SFetch <$ kw "fetch" <*> var <|>
              SUpdate <$ kw "update" <*> var <*> var <|>
              SBlock <$ kw "do" <*> (L.indentGuard sc GT i >>= expr) <|>

              -- FIXME: remove '$' from app syntax, fix 'value' parser with using 'lineFold' instead
              SApp <$> primNameOrDefName <* (optional $ op "$") <*> many var

primNameOrDefName :: Parser Name
primNameOrDefName = nMap ("_"<>) <$ char '_' <*> var <|> var

alternative :: Pos -> Parser Alt
alternative i = Alt <$> try (L.indentGuard sc EQ i *> altPat) <*> (op "@" *> var) <* op "->" <*> (L.indentGuard sc GT i >>= expr)

-- NOTE: The parser `value` already handles the parentheses around "complex" values,
-- and we don't want to parenthesize variables, literals and units.
bindingPat :: Parser BPat
bindingPat =
  VarPat <$> var <|>
  mkAsPat <$> parens ((,) <$> tag <*> many var) <*> (op "@" *> var)
  where mkAsPat (tag, fields) var = AsPat tag fields var


altPat :: Parser CPat
altPat = parens (NodePat <$> tag <*> many var) <|>
         DefaultPat <$ kw "#default" <|>
         LitPat <$> literal

-- #undefined can hold simple types as well as node types
value :: Parser Val
value = Lit <$> literal <|>
        Var <$> var <|>
        try (parens $ ConstTagNode <$> tag <*> many var) <|>
        try (Unit <$ op "()") <|>
        Undefined <$> parens (kw "#undefined" *> op "::" *> typeAnnot)

literal :: Parser Lit
literal = (try $ LFloat . realToFrac <$> signedFloat) <|>
          (try $ LWord64 . fromIntegral <$> lexeme (L.decimal <* C.char 'u')) <|>
          (try $ LInt64 . fromIntegral <$> signedInteger) <|>
          (try $ LBool <$> (True <$ kw "#True" <|> False <$ kw "#False")) <|>
          (try $ LString <$> lexeme (C.char '#' *> quotedString)) <|>
          (try $ LChar <$> lexeme (C.string "#'" *> (escaped <|> anySingle) <* C.char '\''))

satisfyM :: (a -> Bool) -> Parser a -> Parser a
satisfyM pred parser = do
  x <- parser
  if pred x
    then pure x
    else mzero

-- externals

externalBlock = do
  L.indentGuard sc EQ pos1
  ext <- const PrimOp <$> kw "primop" <|> const FFI <$> kw "ffi"
  eff <- const False <$> kw "pure" <|> const True <$> kw "effectful"
  i <- L.indentGuard sc GT pos1
  some $ try (external ext eff i)

external :: ExternalKind -> Bool -> Pos -> Parser External
external ext eff i = do
  L.indentGuard sc EQ i
  name <- var
  L.indentGuard sc GT i >> op "::"
  ty <- reverse <$> sepBy1 (L.indentGuard sc GT i >> L.lexeme sc tyP ) (L.indentGuard sc GT i >> op "->")
  let (retTy:argTyRev) = ty
  pure External
    { eName       = name
    , eRetType    = retTy
    , eArgsType   = reverse argTyRev
    , eEffectful  = eff
    , eKind       = ext
    }

tyP :: Parser Ty
tyP =
  TyVar <$ C.char '%' <*> var <|>
  braces (TyCon <$> var <*> many tyP) <|>
  TySimple <$> try simpleType

-- top-level API

grinModule :: Parser Exp
grinModule = Program <$> (concat <$> many (try externalBlock)) <*> many def <* sc <* eof

parseGrin :: String -> Text -> Either (ParseErrorBundle Text Void) Exp
parseGrin filename content = runParser grinModule filename (withoutTypeAnnots content)

parseProg :: Text -> Exp
parseProg src = either (error . errorBundlePretty) id . parseGrin "" $ withoutTypeAnnots src

parseDef :: Text -> Exp
parseDef src = either (error . errorBundlePretty) id . runParser (def <* sc <* eof) "" $ withoutTypeAnnots src

parseExpr :: Text -> Exp
parseExpr src = either (error . errorBundlePretty) id . runParser (expr pos1 <* sc <* eof) "" $ withoutTypeAnnots src


withoutTypeAnnots :: Text -> Text
withoutTypeAnnots = T.unlines
                  . map skipIfAnnot
                  . T.lines
  where skipIfAnnot line
          | Just ('%',_) <- T.uncons . T.dropWhile isSpace $ line = ""
          | otherwise = line
