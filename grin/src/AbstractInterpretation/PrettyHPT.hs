{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.PrettyHPT where

import Data.Functor.Foldable as Foldable
import Text.PrettyPrint.ANSI.Leijen

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Pretty
import AbstractInterpretation.AbstractRunGrin


instance Pretty Step where
  pretty = \case
    StepExp exp             -> pretty exp
    StepAssign name varset  -> ondullblack $ text name <+> text ":=" <+> pretty varset

instance Pretty a => Pretty (Set a) where
  pretty s = encloseSep lbrace rbrace comma (map pretty $ Set.toList s)

instance Pretty RTLocVal where
  pretty = \case
    RTLoc l     -> int l
    bas@BAS{}   -> text $ show bas
    RTVar name  -> ondullblack $ red $ text name

instance Pretty RTNode where
  pretty (RTNode tag args) = pretty tag <> list (map pretty args)

instance Pretty RTVar where
  pretty = \case
    V value -> pretty value
    N node  -> pretty node

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = vsep [fill 6 (pretty k) <+> text "->" <+> pretty v | (k,v) <- Map.toList m]

instance Pretty v => Pretty (IntMap v) where
  pretty m = vsep [fill 6 (pretty k) <+> text "->" <+> pretty v | (k,v) <- IntMap.toList m]

instance Pretty Computer where
  pretty Computer{..} = vsep
    [ yellow (text "Heap") <$$> indent 4 (pretty storeMap)
    , yellow (text "Env") <$$> indent 4 (pretty envMap)
    , yellow (text "Steps") <$$> indent 4 (foldl (\a b -> a <$$> b <$$> text "--------") empty $ map (pretty) (reverse steps))
    ]