{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module Grin.SyntaxDefs where

import Data.Text (Text, unpack)
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Data
import Data.String
import Text.Printf

-- Names are stored in NM form when we do program generation. NI is only used
-- when we seralize the Exp
data Name
  = NM { unNM :: !Text }
  | NI !Int
  deriving (Generic, Data, NFData, Eq, Ord, Show)

nMap :: (Text -> Text) -> Name -> Name
nMap f (NM n) = NM (f n)

instance Semigroup Name where
  (NM n1) <> (NM n2) = NM (n1 <> n2)

instance Monoid Name where
  mempty = NM mempty

instance IsString Name where
  fromString = NM . fromString

instance PrintfArg Name where
  formatArg = formatString . unpack . unNM

-- * GRIN Tag

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, Data, NFData, Eq, Ord, Show)

data Tag = Tag
  { tagType :: TagType
  , tagName :: Name
  }
  deriving (Generic, Data, NFData, Eq, Ord, Show)

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
  deriving (Generic, Data, NFData, Eq, Ord, Show)
