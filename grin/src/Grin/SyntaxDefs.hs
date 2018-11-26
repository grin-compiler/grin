{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Grin.SyntaxDefs where

import Data.Text.Short (ShortText)
import Control.DeepSeq
import GHC.Generics (Generic)

type Name = ShortText

-- * GRIN Tag

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, NFData, Eq, Ord, Show)

data Tag = Tag
  { tagType :: TagType
  , tagName :: Name
  }
  deriving (Generic, NFData, Eq, Ord, Show)