{-# LANGUAGE TupleSections, LambdaCase #-}

module Frontend.Lambda.Parse (parseLambda) where

import Data.Void
import Control.Applicative (empty)
import Control.Monad (void, mzero)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char as C
import qualified Data.Set as Set
import Frontend.Lambda.Syntax

keywords = Set.fromList ["case","of","let","letrec","letS", "#True", "#False", "_"]

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

var :: Parser String
var = try $ lexeme ((:) <$> lowerChar <*> many (alphaNumChar <|> oneOf "'_")) >>= \x -> case Set.member x keywords of
  True -> fail $ "keyword: " ++ x
  False -> pure x

con :: Parser String
con = lexeme $ some (alphaNumChar)

integer = lexeme L.decimal
signedInteger = L.signed sc' integer

float = lexeme L.float
signedFloat = L.signed sc' float

-- lambda syntax

def :: Parser Def
def = Def <$> try (L.indentGuard sc EQ pos1 *> var) <*> many var <* op "=" <*> (L.indentGuard sc GT pos1 >>= expr)

varBind :: Pos -> Parser (Name, Exp)
varBind i = (,) <$> try (L.indentGuard sc EQ i *> var) <* op "=" <*> (L.indentGuard sc GT i >>= expr)

expr :: Pos -> Parser Exp
expr i = L.indentGuard sc EQ i >>
  Case <$ kw "case"     <*> atom <* kw "of" <*> (L.indentGuard sc GT i >>= some . alternative) <|>
  LetS <$ kw "letS"     <*> (L.indentGuard sc GT i >>= some . varBind) <*> expr i <|>
  LetRec <$ kw "letrec" <*> (L.indentGuard sc GT i >>= some . varBind) <*> expr i <|>
  Let  <$ kw "let"      <*> (L.indentGuard sc GT i >>= some . varBind) <*> expr i <|>
  parens (Con <$> tag <*> many atom) <|>
  try (App <$> primNameOrDefName <*> some atom) <|>
  atom

atom :: Parser Atom
atom = Lit <$> literal <|>
       Var <$> var

primNameOrDefName :: Parser Name
primNameOrDefName = ('_':) <$ char '_' <*> var <|> var

alternative :: Pos -> Parser Alt
alternative i = Alt <$> try (L.indentGuard sc EQ i *> altPat) <* op "->" <*> (L.indentGuard sc GT i >>= expr)

altPat :: Parser Pat
altPat = parens (NodePat <$> tag <*> many var) <|>
         LitPat <$> literal <|>
         DefaultPat <$ kw "_"

tag :: Parser Name
tag = con

literal :: Parser Lit
literal = (try $ LFloat . realToFrac <$> signedFloat) <|>
          (try $ LWord64 . fromIntegral <$> lexeme (L.decimal <* C.char 'u')) <|>
          LInt64 . fromIntegral <$> signedInteger <|>
          LBool <$> (True <$ kw "#True" <|> False <$ kw "#False")


lambdaModule :: Parser Program
lambdaModule = Program <$> some def <* sc <* eof

parseLambda :: String -> String -> Either (ParseError Char Void) Program
parseLambda filename content = runParser lambdaModule filename content
