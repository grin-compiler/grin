{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Grin.SyntaxDefs where

import Control.DeepSeq
import GHC.Generics (Generic)

type Name = String

-- * GRIN Tag

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, NFData, Eq, Ord, Show)

data Tag = Tag
  { tagType :: TagType
  , tagName :: Name
  }
  deriving (Generic, NFData, Eq, Ord, Show)