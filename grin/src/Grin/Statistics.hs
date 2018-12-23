{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Grin.Statistics where

import Data.Monoid
import Data.Functor.Foldable
import qualified Data.Foldable
import Text.PrettyPrint.ANSI.Leijen
import Grin.Grin


data Statistics = Statistics
  { def     :: !Int
  , eBind   :: !Int
  , eCase   :: !Int
  -- Simple Exp
  , sApp    :: !Int
  , sReturn :: !Int
  , sStore  :: !Int
  , sFetchI :: !Int
  , sUpdate :: !Int
  , sBlock  :: !Int
  -- Alt
  , alt     :: !Int
  } deriving (Show)

instance Monoid Statistics where
  mempty = Statistics 0 0 0 0 0 0 0 0 0 0

instance Semigroup Statistics where
  (<>)
    (Statistics a0 b0 c0 d0 e0 f0 g0 h0 i0 j0)
    (Statistics a1 b1 c1 d1 e1 f1 g1 h1 i1 j1)
    = Statistics
        (a0 + a1)
        (b0 + b1)
        (c0 + c1)
        (d0 + d1)
        (e0 + e1)
        (f0 + f1)
        (g0 + g1)
        (h0 + h1)
        (i0 + i1)
        (j0 + j1)

statistics :: Exp -> Statistics
statistics = cata $ \case
  DefF _ _   s -> s <> mempty { def = 1 }
  -- Exp
  EBindF l _ r -> l <> r     <> mempty { eBind = 1 }
  ECaseF _ xs  -> mconcat xs <> mempty { eCase = 1 }
  -- Simple Expr
  SAppF _ _    -> mempty { sApp    = 1 }
  SReturnF _   -> mempty { sReturn = 1 }
  SStoreF  _   -> mempty { sStore  = 1 }
  SFetchIF _ _ -> mempty { sFetchI = 1 }
  SUpdateF _ _ -> mempty { sUpdate = 1 }
  SBlockF  s   -> s <> mempty { sBlock  = 1 }
  -- Alt
  AltF _ s     -> s <> mempty { alt = 1 }
  -- general case
  e -> Data.Foldable.fold e

instance Pretty Statistics where
  pretty (Statistics{..}) = vsep
    [ hsep [yellow $ fill 16 $ text "Definitions:", green $ text $ show def]
    , hsep [yellow $ fill 16 $ text "Binds:", green $ text $ show eBind]
    , hsep [yellow $ fill 16 $ text "Blocks:", green $ text $ show sBlock]
    , hsep [yellow $ fill 16 $ text "Cases:", green $ text $ show eCase]
    , hsep [yellow $ fill 16 $ text "Alternatives:", green $ text $ show alt]
    , hsep [yellow $ fill 16 $ text "Function calls:", green $ text $ show sApp]
    , hsep [yellow $ fill 16 $ text "Returns:", green $ text $ show sReturn]
    , hsep [yellow $ fill 16 $ text "Stores:", green $ text $ show sStore]
    , hsep [yellow $ fill 16 $ text "Fethces:", green $ text $ show sFetchI]
    , hsep [yellow $ fill 16 $ text "Updates:", green $ text $ show sUpdate]
    , yellow $ text "------------------"
    , hsep [yellow $ fill 16 $ text "Summary:", green $ text $ show $ sum
              [def, eBind, eCase, alt, sApp, sReturn, sStore, sFetchI, sUpdate]]
    ]
