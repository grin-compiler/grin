{-# LANGUAGE TupleSections #-}

module ParseGrin where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C
import Text.Show.Pretty (pPrint)
import qualified Data.Set as Set
import Grin

keywords = Set.fromList ["case","of","pure","fetch","store","update","if","then","else","do"]

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

kw w = lexeme $ string w

op w = L.symbol sc' w

var :: Parser String
var = try $ lexeme ((:) <$> lowerChar <*> many (alphaNumChar <|> oneOf "'_")) >>= \x -> case Set.member x keywords of
  True -> fail $ "keyword: " ++ x
  False -> return x

con :: Parser String
con = lexeme $ (:) <$> upperChar <*> many (alphaNumChar)

integer = lexeme L.integer
signedInteger = L.signed sc' integer

float = lexeme L.float
signedFloat = L.signed sc' float

-- grin syntax

def = Def <$> try (L.indentGuard sc EQ (unsafePos 1) *> var) <*> many var <* op "=" <*> (L.indentGuard sc GT (unsafePos 1) >>= expr)

expr i = L.indentGuard sc EQ i >>
  (\pat e b -> EBind e pat b) <$> try (value <* op "<-") <*> simpleExp i <*> expr i <|>
  ECase <$ kw "case" <*> value <* kw "of" <*> (L.indentGuard sc GT i >>= some . alternative) <|>
  ifThenElse i <|>
  try ((\n v e -> EBind (SUpdate n v) Unit e) <$ kw "update" <*> var <*> value <*> expr i) <|>
  simpleExp i

ifThenElse i = do
  kw "if"
  v <- value
  kw "then"
  t <- (L.indentGuard sc GT i >>= expr)
  L.indentGuard sc EQ i
  kw "else"
  e <- (L.indentGuard sc GT i >>= expr)
  return $ ECase v [ Alt (TagPat (Tag C "True"  0)) t
                   , Alt (TagPat (Tag C "False" 0)) e
                   ]

simpleExp i = SReturn <$ kw "pure" <*> value <|>
              SStore <$ kw "store" <*> value <|>
              SFetchI <$ kw "fetch" <*> var <*> optional (between (char '[') (char ']') $ fromIntegral <$> integer) <|>
              SUpdate <$ kw "update" <*> var <*> value <|>
              SBlock <$ kw "do" <*> (L.indentGuard sc GT i >>= expr) <|>
              SApp <$> primNameOrDefName <*> some simpleValue

primNameOrDefName = ('_':) <$ char '_' <*> var <|> var

alternative i = Alt <$> try (L.indentGuard sc EQ i *> altPat) <* op "->" <*> (L.indentGuard sc GT i >>= expr)

altPat = parens (NodePat <$> tag <*> many var) <|>
         TagPat <$> tag <|>
         LitPat <$> literal

tag = Tag C <$ char 'C' <*> con <*> pure 0 <|> -- TODO
      Tag F <$ char 'F' <*> var <*> pure 0 <|> -- TODO
      Tag P <$ char 'P' <*> (var <|> con) <*> pure 0 -- TODO

simpleValue = Lit <$> literal <|>
              Var <$> var

value = Unit <$ op "()" <|>
        parens (ConstTagNode <$> tag <*> many simpleValue) <|>
        parens (VarTagNode <$> var <*> many simpleValue) <|>
        ValTag <$> tag <|>
        simpleValue

literal :: Parser Lit
literal = LInt64 . fromIntegral <$> signedInteger <|>
          LWord64 . fromIntegral <$> lexeme (L.integer <* C.char 'u') <|>
          LFloat . realToFrac <$> signedFloat

--parseFromFile :: Parser _ -> String -> IO _
parseFromFile p file = runParser p file <$> readFile file

--parseGrin :: String -> IO (Either _ [Def])
parseGrin fname = parseFromFile (some def <* sc <* eof) fname
