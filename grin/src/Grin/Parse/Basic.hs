{-# LANGUAGE OverloadedStrings #-}
module Grin.Parse.Basic where

import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Set    as Set

import qualified Data.Vector as Vec

import Data.String (fromString)
import Data.Text (Text)
import Data.Void

import Control.Monad (void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

import Grin.Grin
import Grin.TypeEnvDefs

type Parser = Parsec Void Text

keywords = Set.fromList
  [ "case", "of"
  , "fetch", "store", "update"
  , "if", "then", "else"
  , "do", "pure"
  , "#True", "#False"
  , "#undefined"
  , "primop", "effectful"
  ] `Set.union` simpleTypes

simpleTypes = Set.fromList
  [ "T_Int64", "T_Word64", "T_Float"
  , "T_Bool",  "T_Unit"
  , "T_Location", "T_Dead"
  , "T_String", "T_Char"
  ]

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment

sc' :: Parser ()
sc' = L.space (void $ oneOf (" \t" :: String)) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc'

symbol = L.symbol sc'

parens = between (symbol "(") (symbol ")")

brackets = between (symbol "[") (symbol "]")

braces = between (symbol "{") (symbol "}")

kw w = lexeme $ string w

op w = L.symbol sc' w

int :: Parser Int
int = lexeme L.decimal

integer = lexeme L.decimal
signedInteger = L.signed sc' integer

float = lexeme L.float
signedFloat = L.signed sc' float

list :: Parser a -> Parser [a]
list p = brackets (sepBy p (op ","))

list1 :: Parser a -> Parser [a]
list1 p = brackets (sepBy1 p (op ","))

vec :: Parser a -> Parser (Vector a)
vec p = Vec.fromList <$> list p

vec1 :: Parser a -> Parser (Vector a)
vec1 p = Vec.fromList <$> list1 p

bracedList :: Parser a -> Parser [a]
bracedList p = braces (sepBy p (op ","))

set :: Ord a => Parser a -> Parser (Set a)
set p = Set.fromList <$> bracedList p

set1 :: Ord a => Parser a -> Parser (Set a)
set1 p = Set.fromList <$> bracedList p

anySingle :: MonadParsec e s m => m (Token s)
anySingle = satisfy (const True)

anySingleBut :: MonadParsec e s m => Token s -> m (Token s)
anySingleBut t = satisfy (/= t)


-- grin syntax

escaped :: Parser Char
escaped = string "\\\"" >> pure '"'

quotedVar :: Parser Name
quotedVar = packName <$ char '"' <*> someTill (escaped <|> anyChar) (char '"')

escapedStringChar :: Parser Char
escapedStringChar =
  (string "\\\"" >> pure '"') <|>
  (string "\\\\" >> pure '\\') <|>
  (string "\\a" >> pure '\a') <|>
  (string "\\b" >> pure '\b') <|>
  (string "\\f" >> pure '\f') <|>
  (string "\\n" >> pure '\n') <|>
  (string "\\r" >> pure '\r') <|>
  (string "\\t" >> pure '\t') <|>
  (string "\\v" >> pure '\v')

quotedString :: Parser Text
quotedString = fromString <$> (char '"' *> manyTill (escapedStringChar <|> anyChar) (char '"'))

simpleVar :: Parser Name
simpleVar = (\c s -> packName $ c : s) <$> oneOf allowedInitial <*> many (alphaNumChar <|> oneOf allowedSpecial)

-- TODO: allow keywords in quotes
var :: Parser Name
var = try $ lexeme (quotedVar <|> simpleVar) >>= \x@(NM x') -> case Set.member x' keywords of
  True -> fail $ "keyword: " ++ unpackName x
  False -> return x

tag :: Parser Tag
tag = Tag C <$ char 'C' <*> var <|>
      Tag F <$ char 'F' <*> var <|>
      Tag <$> (P <$ char 'P' <*> L.decimal) <*> var

-- type syntax

simpleType :: Parser SimpleType
simpleType =
  T_Int64 <$ kw "T_Int64" <|>
  T_Word64 <$ kw "T_Word64" <|>
  T_Float <$ kw "T_Float" <|>
  T_Bool <$ kw "T_Bool" <|>
  T_Unit <$ kw "T_Unit" <|>
  T_UnspecifiedLocation <$ kw "#ptr" <|>
  T_Location <$> bracedList int <|>
  T_String <$ kw "T_String" <|>
  T_Char <$ kw "T_Char" <|>
  T_Dead <$ kw "T_Dead"
