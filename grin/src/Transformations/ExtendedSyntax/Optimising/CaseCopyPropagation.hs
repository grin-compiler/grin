{-# LANGUAGE LambdaCase #-}
module Transformations.ExtendedSyntax.Optimising.CaseCopyPropagation where

import Data.Map (Map)
import Data.Functor.Foldable ()
import Data.Foldable

import qualified Data.Map as Map

import Control.Monad.State

import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Util (cataM)


-- NOTE: ~ Maybe Tag
data TagInfo = Unknown | Known Tag
  deriving (Eq, Ord, Show)

-- | Maps alt names to TagInfo
type InfoTable = Map Name TagInfo

collectTagInfo :: Exp -> InfoTable
collectTagInfo = flip execState mempty . cataM alg where

  alg :: ExpF TagInfo -> State InfoTable TagInfo
  alg = \case
    SBlockF tagInfo                  -> pure tagInfo
    EBindF _ _ rhsTagInfo            -> pure rhsTagInfo
    ECaseF scrut altTagInfo          -> pure $ commonTag altTagInfo
    SReturnF (ConstTagNode tag args) -> pure $ Known tag

    AltF _ name tagInfo -> do
      modify (Map.insert name tagInfo)
      pure tagInfo

    _ -> pure Unknown

  commonTag :: [TagInfo] -> TagInfo
  commonTag (t : ts)
    | all (==t) ts = t
  commonTag _ = Unknown
