{-# LANGUAGE LambdaCase #-}
module Statistics where

import Data.Monoid
import Grin
import Data.Functor.Foldable


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
  mappend
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
  ProgramF  xs -> mconcat xs
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
