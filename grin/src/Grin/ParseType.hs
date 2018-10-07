module Grin.ParseType where

import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.Vector as Vec
import Data.Void 

import Control.Monad (void)

import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char as C

import Grin.Grin
import Grin.TypeEnvDefs hiding (simpleType, nodeSet)
import Grin.ParseBasic


simpleType :: Parser SimpleType
simpleType = T_Int64 <$ kw "T_Int64" <|>
             T_Word64 <$ kw "T_Word64" <|>
             T_Float <$ kw "T_Float" <|>
             T_Bool <$ kw "T_Bool" <|>
             T_Unit <$ kw "T_Unit" <|>
             T_Location <$> bracedList int <|>
             T_Dead <$ kw "T_Dead"

nodeType :: Parser (Tag, Vector SimpleType)
nodeType = (,) <$> tag <*> vec simpleType 

nodeSet :: Parser NodeSet 
nodeSet = Map.fromList <$> bracedList nodeType

typeAnnot :: Parser Type 
typeAnnot = try (T_SimpleType <$> simpleType) <|> 
            T_NodeSet <$> nodeSet

functionType :: Parser (Type, Vector Type)
functionType = toPair <$> sepBy1 typeAnnot (op "->")
  where toPair ts = (last ts, Vec.fromList . init $ ts)
