{-# LANGUAGE TupleSections #-}

module ParseGrin where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Set as Set
import Grin
import ReduceGrin

keywords = Set.fromList ["case","of","return","fetch","store","update","if","then","else","do"]

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
  (\pat e b -> Bind e pat b) <$> try (value <* op "<-") <*> simpleExp i <*> expr i <|>
  Case <$ kw "case" <*> value <* kw "of" <*> (L.indentGuard sc GT i >>= some . alternative) <|>
  ifThenElse i <|>
  try ((\n v e -> Bind (Update n v) Unit e) <$ kw "update" <*> var <*> value <*> expr i) <|>
  SExp <$> simpleExp i

ifThenElse i = do
  kw "if"
  v <- value
  kw "then"
  t <- (L.indentGuard sc GT i >>= expr)
  L.indentGuard sc EQ i
  kw "else"
  e <- (L.indentGuard sc GT i >>= expr)
  return $ Case v [ Alt (TagPat (Tag C "True"  0)) t
                  , Alt (TagPat (Tag C "False" 0)) e
                  ]

simpleExp i = Return <$ kw "return" <*> value <|>
              Store <$ kw "store" <*> value <|>
              Fetch <$ kw "fetch" <*> var <|>
              Update <$ kw "update" <*> var <*> value <|>
              Block <$ kw "do" <*> (L.indentGuard sc GT i >>= expr) <|>
              App <$> var <*> some simpleValue

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
        parens (TagNode <$> tag <*> many simpleValue) <|>
        parens (VarNode <$> var <*> many simpleValue) <|>
        ValTag <$> tag <|>
        simpleValue

literal :: Parser Lit
literal = LFloat . realToFrac <$> try signedFloat <|> LFloat . fromIntegral <$> signedInteger

parseFromFile p file = runParser p file <$> readFile file

eval :: String -> IO ()
eval fname = do
  result <- parseFromFile (some def <* sc <* eof) fname
  case result of
    Left err -> print err
    Right e  -> do
      print e
      putStrLn "-------"
      print $ reduceFun e "main"
