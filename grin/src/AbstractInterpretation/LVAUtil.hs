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

isNodeLive :: Node -> Bool
isNodeLive = (||) <$> hasLiveTag <*> hasLiveField

hasLiveTag :: Node -> Bool
hasLiveTag (Node tagLv fieldsLv) = tagLv

hasLiveField :: Node -> Bool
hasLiveField (Node tagLv fieldsLv) = or fieldsLv

isLive :: Liveness -> Bool
isLive (BasicVal b) = b
isLive (NodeSet  m) = any isNodeLive m

-- | A function is only dead if its return value is dead
-- , and all of its parameters are dead as well. The case
-- when the return value is dead, but there is a live parameter
-- means that the function has some kind of side effect.
isFunDead :: (Liveness, Vector Liveness) -> Bool
isFunDead (retLv, argsLv) = not (isLive retLv || any isLive argsLv)