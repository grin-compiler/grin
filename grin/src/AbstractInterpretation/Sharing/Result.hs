{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, RecordWildCards #-}

module AbstractInterpretation.Sharing.Result where

import Data.Set    (Set)
import Data.Map    (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Lens.Micro.Platform
import Lens.Micro.Internal

import Grin.Grin (Name, Tag)
import AbstractInterpretation.Sharing.CodeGen
import AbstractInterpretation.HeapPointsTo.Result
import qualified AbstractInterpretation.Reduce as R


data SharingResult
  = SharingResult
  { _hptResult  :: HPTResult
  , _sharedLocs :: Set Loc
  } deriving (Show)

concat <$> mapM makeLenses [''SharingResult]

toSharingResult :: SharingProgram -> R.Computer -> SharingResult
toSharingResult SharingProgram{..} comp = SharingResult hptResult' sharedLocs where
  hptResult  = toHPTResult _hptProg comp
  hptResult' = register %~ (Map.delete sharingRegisterName) $ hptResult
  shRegType  = Map.lookup sharingRegisterName . _register $ hptResult
  sharedLocs = case shRegType of
    Just (TypeSet sty _) -> onlyLocations sty
    Nothing -> error $ "Sharing register not found (" ++ show sharingRegisterName ++ ")"

  onlyLocations :: Set SimpleType -> Set Loc
  onlyLocations stys = Set.fromList [ l | T_Location l <- Set.toList stys ]
