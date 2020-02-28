{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Grin.Parse.TypeEnv
  ( typeAnnot
  , parseTypeEnv
  , parseMarkedTypeEnv
  , parseMarkedTypeEnv'
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vec

import Data.List
import Data.Char
import Data.Void
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad (void)

import Lens.Micro.Platform

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char as C

import Grin.Grin
import Grin.TypeEnvDefs hiding (location, nodeSet, simpleType)
import qualified Grin.TypeEnvDefs as Env
import Grin.Parse.Basic

import Control.Monad.State


data TypeEnvEntry
  = Location Int NodeSet
  | Variable Name Type
  | Function Name (Type, Vector Type)
  deriving (Eq, Ord, Show)

nodeType :: Parser (Tag, Vector SimpleType)
nodeType = (,) <$> tag <*> vec simpleType

nodeSet :: Parser NodeSet
nodeSet = Map.fromList <$> bracedList nodeType

typeAnnot :: Parser Type
typeAnnot = try (T_SimpleType <$> simpleType) <|>
            T_NodeSet <$> nodeSet

functionTypeAnnot :: Parser (Type, Vector Type)
functionTypeAnnot = toPair <$> sepBy1 typeAnnot (op "->")
  where toPair ts = (last ts, Vec.fromList . init $ ts)


location :: Parser TypeEnvEntry
location = Location <$> int <* op "->" <*> nodeSet

varType :: Parser TypeEnvEntry
varType = Variable <$> var <* op "->" <*> typeAnnot

functionType :: Parser TypeEnvEntry
functionType = Function <$> var <* op "::" <*> functionTypeAnnot

typeEnvEntry :: Parser TypeEnvEntry
typeEnvEntry = location <|> try varType <|> functionType

markedTypeEnvEntry :: Parser TypeEnvEntry
markedTypeEnvEntry = op "%" *> typeEnvEntry

typeEnvEntries :: Parser [TypeEnvEntry]
typeEnvEntries = many $ typeEnvEntry <* sc

markedTypeEnvEntries :: Parser [TypeEnvEntry]
markedTypeEnvEntries = many $ markedTypeEnvEntry <* sc

typeEnv :: Parser TypeEnv
typeEnv = entriesToTypeEnv <$>
            (sc *> header "Location" *> many' location) <>
                  (header "Variable" *> many' (try varType)) <>
                  (header "Function" *> many' functionType)
            <* eof
  where header w = L.lexeme sc $ string w
        many'  p = many $ L.lexeme sc p

markedTypeEnv :: Parser TypeEnv
markedTypeEnv = entriesToTypeEnv <$> (sc *> markedTypeEnvEntries <* eof)


filterSortLocEntries :: [TypeEnvEntry] -> [TypeEnvEntry]
filterSortLocEntries = sortBy cmpLoc . filter isLoc
  where isLoc (Location _ _) = True
        isLoc _ = False

        cmpLoc (Location n _) (Location m _) = compare n m

locEntriesToHeapMap :: [TypeEnvEntry] -> Vector NodeSet
locEntriesToHeapMap entries = flip execState mempty $ forM entries' $
  \(Location _ t) -> modify $ flip Vec.snoc t
  where entries' = filterSortLocEntries entries

entriesToTypeEnv :: [TypeEnvEntry] -> TypeEnv
entriesToTypeEnv xs = flip execState emptyTypeEnv $ do
  Env.location .= locEntriesToHeapMap xs
  forM_ xs $ \case
    Variable n t  -> variable %= Map.insert n t
    Function n ts -> function %= Map.insert n ts
    Location _ _  -> pure ()

-- parses a type environment (without code)
parseTypeEnv :: Text -> TypeEnv
parseTypeEnv src = either (error . errorBundlePretty) id
                 . runParser typeEnv ""
                 $ src

-- parses type marked type annotations (even interleaved with code)
parseMarkedTypeEnv' :: Text -> TypeEnv
parseMarkedTypeEnv' src = either (error . errorBundlePretty) id $ parseMarkedTypeEnv "" src

parseMarkedTypeEnv :: String -> Text -> Either (ParseErrorBundle Text Void) TypeEnv
parseMarkedTypeEnv filename src = runParser markedTypeEnv filename (withoutCodeLines src)

withoutCodeLines :: Text -> Text
withoutCodeLines = T.unlines
                 . map skipIfCode
                 . T.lines
  where skipIfCode line
          | Just ('%',_) <- T.uncons . T.dropWhile isSpace $ line = line
          | otherwise = ""

