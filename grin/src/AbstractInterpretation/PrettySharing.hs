{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.PrettySharing where

import Text.PrettyPrint.ANSI.Leijen

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Vector (Vector)
import qualified Data.Vector as V

import Grin.Pretty
import Grin.TypeEnvDefs
import AbstractInterpretation.Sharing


prettySharingResult :: TypeEnv -> SharingResult -> Doc
prettySharingResult TypeEnv{..} shLocs = yellow (text "Heap (* is shared)") <$$> 
  (indent 4 . prettyKeyValue . V.toList . V.imap annotateSharedLoc $ _location) 
  where 
    annotateSharedLoc loc ty
      | Set.member loc shLocs = (pretty loc <> text "*", T_NodeSet ty)  
      | otherwise             = (pretty loc            , T_NodeSet ty)  

