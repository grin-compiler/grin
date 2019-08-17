{-# LANGUAGE TupleSections, LambdaCase, OverloadedStrings #-}

module Grin.Parse.AST
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

import Grin.Grin hiding (appName)
import Grin.Parse.Basic
import Grin.Parse.TypeEnv

-- grin syntax

def :: Parser Def
def = Def <$> try (L.indentGuard sc EQ pos1 *> var) <*> many var <* op "=" <*> (L.indentGuard sc GT pos1 >>= expr)

expr :: Pos -> Parser Exp
expr i = L.indentGuard sc EQ i >>
  try ((\pat e b -> EBind e pat b) <$> try (bindingPat <* op "<-") <*> simpleExp i <*> expr i ) <|>
  ifThenElse i <|>
  simpleExp i

ifThenElse :: Pos -> Parser Exp
ifThenElse i = do
  kw "if"
  b <- var
  kw "then"
  t <- (L.indentGuard sc GT i >>= expr)
  L.indentGuard sc EQ i
  kw "else"
  e <- (L.indentGuard sc GT i >>= expr)
  return $ ECase b [ Alt (LitPat (LBool True))  t
                   , Alt (LitPat (LBool False)) e
                   ]

simpleExp :: Pos -> Parser SimpleExp
simpleExp i = SReturn <$ kw "pure" <*> value <|>
              ECase <$ kw "case" <*> var <* kw "of" <*> (L.indentGuard sc GT i >>= some . alternative) <|>
              SStore <$ kw "store" <*> var <|>
              SFetchI <$ kw "fetch" <*> var <*> optional (between (char '[') (char ']') $ fromIntegral <$> integer) <|>
              SUpdate <$ kw "update" <*> var <*> var <|>
              SBlock <$ kw "do" <*> (L.indentGuard sc GT i >>= expr) <|>

              -- FIXME: remove '$' from app syntax, fix 'value' parser with using 'lineFold' instead
              SApp <$> appName <* (optional $ op "$") <*> many var

funName :: Parser Name
funName = var

extName :: Parser Name
extName = nMap ("_" <>) <$ char '_' <*> var

-- TODO: Revisit function application name parsing.
--       Could use some sort of state for parsing (external list?).
--       Naming convention for externals: always begins with underscore.
appName :: Parser AppName
appName = Ext <$> extName <|>
          Fun <$> funName

alternative :: Pos -> Parser Alt
alternative i = Alt <$> try (L.indentGuard sc EQ i *> altPat) <* op "->" <*> (L.indentGuard sc GT i >>= expr)

bindingPat :: Parser BPat
bindingPat = AsPat  <$> (var <* char '@') <*> parens value <|>
             VarPat <$> var

altPat :: Parser CPat
altPat = parens (NodePat <$> tag <*> many var) <|>
         DefaultPat <$ kw "#default" <|>
         TagPat <$> tag <|>
         LitPat <$> literal

-- #undefined can hold simple types as well as node types
value :: Parser Val
value = Lit <$> literal <|>
        Var <$> var <|>
        Unit <$ op "()" <|>
        try (parens (ConstTagNode <$> tag <*> many var <|> VarTagNode <$> var <*> many var)) <|>
        ValTag <$> tag <|>
        Undefined <$> parens (kw "#undefined" *> op "::" *> typeAnnot)

literal :: Parser Lit
literal = (try $ LFloat . realToFrac <$> signedFloat) <|>
          (try $ LWord64 . fromIntegral <$> lexeme (L.decimal <* C.char 'u')) <|>
          (try $ LInt64 . fromIntegral <$> signedInteger) <|>
          (try $ LBool <$> (True <$ kw "#True" <|> False <$ kw "#False")) <|>
          (try $ LString <$> lexeme (C.char '#' *> quotedString)) <|>
          (try $ LChar <$> lexeme (C.string "#'" *> (escaped <|> anyChar) <* C.char '\''))

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

parseGrin :: String -> Text -> Either (ParseError Char Void) Exp
parseGrin filename content = runParser grinModule filename (withoutTypeAnnots content)

parseProg :: Text -> Exp
parseProg src = either (error . parseErrorPretty' src) id . parseGrin "" $ withoutTypeAnnots src

parseDef :: Text -> Exp
parseDef src = either (error . parseErrorPretty' src) id . runParser def "" $ withoutTypeAnnots src

parseExpr :: Text -> Exp
parseExpr src = either (error . parseErrorPretty' src) id . runParser (expr pos1) "" $ withoutTypeAnnots src


withoutTypeAnnots :: Text -> Text
withoutTypeAnnots = T.unlines
                  . map skipIfAnnot
                  . T.lines
  where skipIfAnnot line
          | Just ('%',_) <- T.uncons . T.dropWhile isSpace $ line = ""
          | otherwise = line
