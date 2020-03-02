module Grin.Parse
  ( module Grin.Parse.AST
  , module Grin.Parse.TypeEnv
  , module Grin.Parse
  ) where

import Data.Void
import Data.Text
import Text.Megaparsec

import Grin.Grin
import Grin.TypeEnvDefs

import Grin.Parse.AST
import Grin.Parse.TypeEnv

parseGrinWithTypes :: String -> Text -> Either (ParseErrorBundle Text Void) (TypeEnv, Exp)
parseGrinWithTypes filename content = (,) <$> parseMarkedTypeEnv filename content <*> parseGrin filename content
