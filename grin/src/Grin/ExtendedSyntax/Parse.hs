module Grin.ExtendedSyntax.Parse
  ( module Grin.ExtendedSyntax.Parse.AST
  , module Grin.ExtendedSyntax.Parse.TypeEnv
  , module Grin.ExtendedSyntax.Parse
  ) where

import Data.Void
import Data.Text
import Text.Megaparsec

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TypeEnvDefs

import Grin.ExtendedSyntax.Parse.AST
import Grin.ExtendedSyntax.Parse.TypeEnv

parseGrinWithTypes :: String -> Text -> Either (ParseErrorBundle Text Void) (TypeEnv, Exp)
parseGrinWithTypes filename content = (,) <$> parseMarkedTypeEnv filename content <*> parseGrin filename content
