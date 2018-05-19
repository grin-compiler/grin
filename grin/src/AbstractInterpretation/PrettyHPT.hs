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
    [ yellow (text "Heap") <$$> indent 4 (prettyKeyValue $ IntMap.toList storeMap)
    , yellow (text "Env") <$$> indent 4 (prettyKeyValue $ Map.toList envMap)
    , yellow (text "Steps") <$$> indent 4 (foldl (\a b -> a <$$> b <$$> text "--------") empty $ map (pretty) (reverse steps))
    ]

-- HPT Result NEW

instance Pretty R.SimpleType where
  pretty = \case
    R.T_Location l  -> cyan . int $ fromIntegral l
    ty              -> red $ text $ show ty

prettyHPTNode :: (Tag, Vector (Set R.SimpleType)) -> Doc
prettyHPTNode (tag, args) = pretty tag <> list (map pretty $ V.toList args)

prettyHPTFunction :: (Name, (R.TypeSet, Vector R.TypeSet)) -> Doc
prettyHPTFunction (name, (ret, args)) = text name <> align (encloseSep (text " :: ") empty (text " -> ") (map pretty $ (V.toList args) ++ [ret]))

instance Pretty R.NodeSet where
  pretty (R.NodeSet m) = encloseSep lbrace rbrace comma (map prettyHPTNode $ Map.toList m)

instance Pretty R.TypeSet where
  pretty (R.TypeSet ty (R.NodeSet ns)) = encloseSep lbrace rbrace comma (map prettyHPTNode (Map.toList ns) ++ map pretty (Set.toList ty))

instance Pretty R.HPTResult where
  pretty R.HPTResult{..} = vsep
    [ yellow (text "Heap") <$$> indent 4 (prettyKeyValue $ zip [(0 :: Int)..] $ V.toList _memory)
    , yellow (text "Env") <$$> indent 4 (prettyKeyValue $ Map.toList  _register)
    , yellow (text "Function") <$$> indent 4 (vsep $ map prettyHPTFunction $ Map.toList _function)
    ]

