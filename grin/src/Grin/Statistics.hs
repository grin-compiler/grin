{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Grin.Statistics where

import Data.Monoid
import Data.Functor.Foldable
import qualified Data.Foldable
import Text.PrettyPrint.ANSI.Leijen
import Grin.Grin
import qualified Data.Set as Set


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
  , vars    :: !(Set.Set Name)
  , tags    :: !(Set.Set Tag)
  } deriving (Show)

instance Monoid Statistics where
  mempty = Statistics 0 0 0 0 0 0 0 0 0 0 mempty mempty

instance Semigroup Statistics where
  (<>)
    (Statistics a0 b0 c0 d0 e0 f0 g0 h0 i0 j0 k0 l0)
    (Statistics a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1)
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
        (k0 <> k1)
        (l0 <> l1)

statistics :: Exp -> Statistics
statistics = cata $ \case
  DefF fn ps   s -> s <> mempty { def = 1, vars = Set.singleton fn <> Set.fromList ps }
  -- Exp
  EBindF l b r -> l <> r     <> mempty { eBind = 1, vars = foldNames Set.singleton b, tags = tagInVal b }
  ECaseF v xs  -> mconcat xs <> mempty { eCase = 1, vars = foldNames Set.singleton v, tags = tagInVal v }
  -- Simple Expr
  SAppF f ps   -> mempty { sApp    = 1, vars = Set.singleton f <> foldNames Set.singleton ps }
  SReturnF v   -> mempty { sReturn = 1, vars = foldNames Set.singleton v, tags = tagInVal v }
  SStoreF  v   -> mempty { sStore  = 1, vars = foldNames Set.singleton v, tags = tagInVal v }
  SFetchIF v _ -> mempty { sFetchI = 1, vars = Set.singleton v }
  SUpdateF v x -> mempty { sUpdate = 1, vars = Set.singleton v <> foldNames Set.singleton x, tags = tagInVal x }
  SBlockF  s   -> s <> mempty { sBlock  = 1 }
  -- Alt
  AltF p s     -> s <> mempty { alt = 1, vars = foldNames Set.singleton p, tags = tagInCPat p }
  -- general case
  e -> Data.Foldable.fold e

tagInVal :: Val -> Set.Set Tag
tagInVal = \case
  ConstTagNode t _ -> Set.singleton t
  _                -> Set.empty

tagInCPat :: CPat -> Set.Set Tag
tagInCPat = \case
  NodePat t _ -> Set.singleton t
  TagPat  t   -> Set.singleton t
  _           -> Set.empty

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
    , hsep [yellow $ fill 16 $ text "Node summary:", green $ text $ show $ sum
              [def, eBind, eCase, alt, sApp, sReturn, sStore, sFetchI, sUpdate]]
    , text ""
    , hsep [yellow $ fill 16 $ text "Variables:", green $ text $ show $ Set.size vars]
    , hsep [yellow $ fill 16 $ text "Tags:", green $ text $ show $ Set.size tags]
    ]
