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

-- * GRIN Type System

type Loc = Int

data SimpleType
  = T_Int64
  | T_Word64
  | T_Float
  | T_Bool
  | T_Unit
  | T_Location {_locations :: [Loc]}
  | T_UnspecifiedLocation
  | T_Dead
  | T_String
  | T_Char
  deriving (Generic, NFData, Eq, Ord, Show)
