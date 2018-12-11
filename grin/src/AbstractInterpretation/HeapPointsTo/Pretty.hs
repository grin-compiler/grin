{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.HeapPointsTo.Pretty where

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

import Grin.Grin (Tag, Name)
import Grin.Pretty
import qualified AbstractInterpretation.HeapPointsTo.Result as R

-- HPT Result NEW

instance Pretty R.SimpleType where
  pretty = \case
    R.T_UnspecifiedLocation -> red $ text "#ptr"
    R.T_Location l          -> cyan . int $ fromIntegral l
    ty                      -> red $ text $ show ty

prettyHPTNode :: (Tag, Vector (Set R.SimpleType)) -> Doc
prettyHPTNode (tag, args) = pretty tag <> list (map pretty $ V.toList args)

prettyHPTFunction :: (Name, (R.TypeSet, Vector R.TypeSet)) -> Doc
prettyHPTFunction = prettyFunction

instance Pretty R.NodeSet where
  pretty (R.NodeSet m) = prettyBracedList (map prettyHPTNode $ Map.toList m)

instance Pretty R.TypeSet where
  pretty (R.TypeSet ty (R.NodeSet ns)) = prettyBracedList (map prettyHPTNode (Map.toList ns) ++ map pretty (Set.toList ty))

instance Pretty R.HPTResult where
  pretty R.HPTResult{..} = vsep
    [ yellow (text "Heap") <$$> indent 4 (prettyKeyValue $ zip [(0 :: Int)..] $ V.toList _memory)
    , yellow (text "Env") <$$> indent 4 (prettyKeyValue $ Map.toList  _register)
    , yellow (text "Function") <$$> indent 4 (vsep $ map prettyHPTFunction $ Map.toList _function)
    ]
