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

import Data.Vector (Vector)
import qualified Data.Vector as V

import Grin (Tag, Name)
import Pretty
import AbstractInterpretation.HPTResult
import qualified AbstractInterpretation.HPTResultNew as R


instance Pretty a => Pretty (Set a) where
  pretty s = encloseSep lbrace rbrace comma (map pretty $ Set.toList s)

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = vsep [fill 6 (pretty k) <+> text "->" <+> pretty v | (k,v) <- Map.toList m]

instance Pretty v => Pretty (IntMap v) where
  pretty m = vsep [fill 6 (pretty k) <+> text "->" <+> pretty v | (k,v) <- IntMap.toList m]

instance Pretty v => Pretty (Vector v) where
  pretty m = vsep [fill 6 (int k) <+> text "->" <+> pretty v | (k,v) <- zip [1..] $ V.toList m]

-- HPT Result
instance Pretty Step where
  pretty = \case
    StepExp exp             -> pretty exp
    StepAssign name varset  -> ondullblack $ text name <+> text ":=" <+> pretty varset

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

instance Pretty Computer where
  pretty Computer{..} = vsep
    [ yellow (text "Heap") <$$> indent 4 (pretty storeMap)
    , yellow (text "Env") <$$> indent 4 (pretty envMap)
    , yellow (text "Steps") <$$> indent 4 (foldl (\a b -> a <$$> b <$$> text "--------") empty $ map (pretty) (reverse steps))
    ]

-- HPT Result NEW

instance Pretty R.LocOrValue where
  pretty = \case
    R.Loc l         -> cyan . int . succ $ fromIntegral l
    R.SimpleType ty -> red $ text $ show ty

prettyNode :: (Tag, Vector (Set R.LocOrValue)) -> Doc
prettyNode (tag, args) = pretty tag <> list (map pretty $ V.toList args)

prettyFunction :: (Name, (R.Value, Vector R.Value)) -> Doc
prettyFunction (name, (ret, args)) = text name <> align (encloseSep (text " :: ") empty (text " -> ") (map pretty $ (V.toList args) ++ [ret]))

instance Pretty R.NodeSet where
  pretty (R.NodeSet m) = encloseSep lbrace rbrace comma (map prettyNode $ Map.toList m)

instance Pretty R.Value where
  pretty (R.Value ty (R.NodeSet ns)) = encloseSep lbrace rbrace comma (map prettyNode (Map.toList ns) ++ map pretty (Set.toList ty))

instance Pretty R.HPTResult where
  pretty R.HPTResult{..} = vsep
    [ yellow (text "Heap") <$$> indent 4 (pretty _memory)
    , yellow (text "Env") <$$> indent 4 (pretty _register)
    , yellow (text "Function") <$$> indent 4 (vsep $ map prettyFunction $ Map.toList _function)
    ]

