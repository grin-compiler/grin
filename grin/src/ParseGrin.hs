{-# LANGUAGE TupleSections, LambdaCase #-}

module ParseGrin (parseGrin, parseProg, parseDef, parseExpr) where

import Data.Void
import Control.Applicative (empty)
import Control.Monad (void, mzero)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char as C
import qualified Data.Set as Set
import Grin

keywords = Set.fromList ["case","of","pure","fetch","store","update","if","then","else","do", "#True", "#False"]

type Parser = Parsec Void String

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

-- TODO: unify var and con + support quotted syntax which allow any character
var :: Parser String
var = try $ lexeme ((:) <$> lowerChar <*> many (alphaNumChar <|> oneOf "'_.:!@{}$-")) >>= \x -> case Set.member x keywords of
  True -> fail $ "keyword: " ++ x
  False -> return x

con :: Parser String
con = lexeme $ some (alphaNumChar <|> oneOf "_.{}")

integer = lexeme L.decimal
signedInteger = L.signed sc' integer

float = lexeme L.float
signedFloat = L.signed sc' float

-- grin syntax

def = Def <$> try (L.indentGuard sc EQ pos1 *> var) <*> many var <* op "=" <*> (L.indentGuard sc GT pos1 >>= expr)

expr i = L.indentGuard sc EQ i >>
  try ((\pat e b -> EBind e pat b) <$> (try (value <* op "<-") <|> pure Unit) <*> simpleExp i <*> expr i ) <|>
  ifThenElse i <|>
  simpleExp i

ifThenElse i = do
  kw "if"
  v <- value
  kw "then"
  t <- (L.indentGuard sc GT i >>= expr)
  L.indentGuard sc EQ i
  kw "else"
  e <- (L.indentGuard sc GT i >>= expr)
  return $ ECase v [ Alt (LitPat (LBool True))  t
                   , Alt (LitPat (LBool False)) e
                   ]

simpleExp i = SReturn <$ kw "pure" <*> value <|>
              ECase <$ kw "case" <*> value <* kw "of" <*> (L.indentGuard sc GT i >>= some . alternative) <|>
              SStore <$ kw "store" <*> satisfyM nodeOrVar value <|>
              SFetchI <$ kw "fetch" <*> var <*> optional (between (char '[') (char ']') $ fromIntegral <$> integer) <|>
              SUpdate <$ kw "update" <*> var <*> satisfyM nodeOrVar value <|>
              SBlock <$ kw "do" <*> (L.indentGuard sc GT i >>= expr) <|>
              SApp <$> primNameOrDefName <*> some simpleValue
  where
    nodeOrVar = \case
      ConstTagNode _ _ -> True
      VarTagNode _ _   -> True
      Var _            -> True
      _                -> False

primNameOrDefName = ('_':) <$ char '_' <*> var <|> var

alternative i = Alt <$> try (L.indentGuard sc EQ i *> altPat) <* op "->" <*> (L.indentGuard sc GT i >>= expr)

altPat = parens (NodePat <$> tag <*> many var) <|>
         DefaultPat <$ kw "#default" <|>
         TagPat <$> tag <|>
         LitPat <$> literal

tag = Tag C <$ char 'C' <*> con <|>
      Tag F <$ char 'F' <*> var <|>
      Tag <$> (P <$ char 'P' <*> L.decimal) <*> (var <|> con)

simpleValue = Lit <$> literal <|>
              Var <$> var

value = Unit <$ op "()" <|>
        parens (ConstTagNode <$> tag <*> many simpleValue <|> VarTagNode <$> var <*> many simpleValue) <|>
        ValTag <$> tag <|>
        simpleValue

literal :: Parser Lit
literal = (try $ LFloat . realToFrac <$> signedFloat) <|>
          (try $ LWord64 . fromIntegral <$> lexeme (L.decimal <* C.char 'u')) <|>
          LInt64 . fromIntegral <$> signedInteger <|>
          LBool <$> (True <$ kw "#True" <|> False <$ kw "#False")

satisfyM :: (a -> Bool) -> Parser a -> Parser a
satisfyM pred parser = do
  x <- parser
  if pred x
    then pure x
    else mzero


grinModule :: Parser Exp
grinModule = Program <$> some def <* sc <* eof

parseGrin :: String -> String -> Either (ParseError Char Void) Exp
parseGrin filename content = runParser grinModule filename content

parseProg :: String -> Exp
parseProg src = either (error . parseErrorPretty' src) id . parseGrin "" $ src

parseDef :: String -> Exp
parseDef src = either (error . parseErrorPretty' src) id . runParser def "" $ src

parseExpr :: String -> Exp
parseExpr src = either (error . parseErrorPretty' src) id . runParser (expr pos1) "" $ src
