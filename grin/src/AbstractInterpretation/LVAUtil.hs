{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies, DeriveFunctor, ViewPatterns #-}
module AbstractInterpretation.LVAUtil
( module AbstractInterpretation.LVAUtil
, module AbstractInterpretation.LVAResultTypes
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap

import AbstractInterpretation.LVAResultTypes

hasLiveField :: Node -> Bool 
hasLiveField (Node liveness) = or liveness

isLive :: Liveness -> Bool 
isLive (BasicVal b) = b 
isLive (NodeSet  m) = any hasLiveField m