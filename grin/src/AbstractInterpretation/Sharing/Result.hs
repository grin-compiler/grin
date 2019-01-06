{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RecordWildCards #-}

module AbstractInterpretation.Sharing.Result where

import Data.Set    (Set)
import Data.Map    (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Lens.Micro.Platform

import Grin.Grin (Name, Tag)
import AbstractInterpretation.IR (AbstractProgram)
import AbstractInterpretation.Sharing.CodeGen
import AbstractInterpretation.HeapPointsTo.Result
import qualified AbstractInterpretation.Reduce as R


data SharingResult
  = SharingResult
  { _hptResult  :: HPTResult
  , _sharedLocs :: Set Loc
  }
  deriving (Eq, Show)

emptySharingResult :: SharingResult
emptySharingResult = SharingResult emptyHPTResult mempty

concat <$> mapM makeLenses [''SharingResult]

toSharingResult :: SharingMapping -> R.ComputerState -> SharingResult
toSharingResult SharingMapping{..} comp = SharingResult hptResult sharedLocs where
  hptResult  = toHPTResult _hptMapping comp
  sharedLocs = onlyLocations sty
  TypeSet sty _ = convertReg (_hptMapping, comp) _shRegName

  onlyLocations :: Set SimpleType -> Set Loc
  onlyLocations stys = Set.fromList [ l | T_Location l <- Set.toList stys ]
