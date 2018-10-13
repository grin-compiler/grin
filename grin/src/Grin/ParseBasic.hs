module Grin.ParseBasic where

import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Set    as Set
import qualified Data.Vector as Vec
import Data.Void 

import Control.Monad (void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

import Grin.Grin
import Grin.TypeEnvDefs

type Parser = Parsec Void String

keywords = Set.fromList 
  [ "case", "of"
  , "fetch", "store", "update"
  , "if", "then", "else"
  , "do", "pure"
  , "#True", "#False"
  , "#undefined"
  ] `Set.union` simpleTypes

simpleTypes = Set.fromList 
  [ show T_Int64, show T_Word64, show T_Float
  , show T_Bool,  show T_Unit
  , "T_Location", show T_Dead
  ]

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment

sc' :: Parser ()
sc' = L.space (void $ oneOf " \t") lineComment blockComment

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

-- TODO: unify var and con + support quotted syntax which allow any character
var :: Parser String
var = try $ lexeme ((:) <$> lowerChar <*> many (alphaNumChar <|> oneOf "'_.:!@{}$-")) >>= \x -> case Set.member x keywords of
  True -> fail $ "keyword: " ++ x
  False -> return x

con :: Parser String
con = lexeme $ some (alphaNumChar <|> oneOf "_.{}")

tag :: Parser Tag
tag = Tag C <$ char 'C' <*> con <|>
      Tag F <$ char 'F' <*> var <|>
      Tag <$> (P <$ char 'P' <*> L.decimal) <*> (var <|> con)