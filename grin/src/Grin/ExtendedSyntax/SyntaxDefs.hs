{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, StandaloneDeriving, LambdaCase #-}
module Grin.ExtendedSyntax.SyntaxDefs where

import Data.Text (Text, unpack)
import Data.Binary
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Data
import Data.String
import Text.Printf
import Lens.Micro.Platform

-- Names are stored in NM form when we do program generation. NI is only used
-- when we seralize the Exp
data Name
  = NM { unNM :: !Text }
  | NI !Int
  deriving (Generic, Data, NFData, Binary, Eq, Ord, Show)

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

nameString :: Name -> String
nameString = \case
  NM n -> unpack n
  _    -> error "Name index found." -- This could have left in the AST after a problematic deserialisation.

-- * GRIN Tag

data TagType = C | F | P Int {-missing parameter count-}
  deriving (Generic, Data, NFData, Binary, Eq, Ord, Show)

data Tag = Tag
  { tagType :: TagType
  , tagName :: Name
  }
  deriving (Generic, Data, NFData, Binary, Eq, Ord, Show)

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
  deriving (Generic, Data, NFData, Binary, Eq, Ord, Show)

-- * Traversals

_NM :: Traversal' Name Text
_NM f (NM n) = NM <$> f n
_NM _ other  = pure other

_NI :: Traversal' Name Int
_NI f (NI i) = NI <$> f i
_NI _ other  = pure other
