{-# LANGUAGE LambdaCase #-}
module Transformations.ExtendedSyntax.Optimising.CaseCopyPropagation where

import Data.Map (Map)
import Data.Functor.Foldable

import qualified Data.Map as Map

import Control.Monad.State

import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Names
import Transformations.ExtendedSyntax.Util (cataM)


-- NOTE: ~ Maybe Tag
data TagInfo = Unknown | Known Tag
  deriving (Eq, Ord, Show)

-- | Maps alt names to TagInfo
type InfoTable = Map Name TagInfo

-- NOTE: Case Copy Propagtion ~ Case Unboxing
caseCopyPropagation :: Exp -> (Exp, ExpChanges)
caseCopyPropagation e = rebindCases infoTable e where
  infoTable = collectTagInfo e

-- | Collects tag information about case alternatives.
collectTagInfo :: Exp -> InfoTable
collectTagInfo = flip execState mempty . cataM alg where

  alg :: ExpF TagInfo -> State InfoTable TagInfo
  alg = \case
    SBlockF tagInfo                   -> pure tagInfo
    EBindF _ _ rhsTagInfo             -> pure rhsTagInfo
    ECaseF scrut altTagInfo           -> pure $ commonTag altTagInfo
    SReturnF (ConstTagNode tag [arg]) -> pure $ Known tag

    AltF _ name tagInfo -> do
      modify (Map.insert name tagInfo)
      pure tagInfo

    _ -> pure Unknown

-- | Rebinds unboxable case expressions, and unboxes
-- the corresponding alternatives' last return expressions.
rebindCases :: InfoTable -> Exp -> (Exp, ExpChanges)
rebindCases infoTable e = evalNameM e $ cataM alg e where

  alg :: ExpF Exp -> NameM Exp
  alg = \case
    ECaseF scrut alts
      | Known tag <- lookupCommonTag [ name | Alt _ name _ <- alts ]
      , alts'     <- [ Alt cpat name (unboxLastReturn body) | Alt cpat name body <- alts ]
      , case'     <- ECase scrut alts'
      -> do
        res <- deriveNewName "ccp"
        pure $ SBlock $ EBind case' (VarPat res) (SReturn $ ConstTagNode tag [res])
    e -> pure $ embed e

  -- | Determine the common tag for a set of alternatives (if it exists).
  lookupCommonTag :: [Name] -> TagInfo
  lookupCommonTag =
    commonTag
    . map (\alt -> Map.findWithDefault Unknown alt infoTable)

-- | Unboxes the last node-returning expression in a binding sequence.
unboxLastReturn :: Exp -> Exp
unboxLastReturn = apo coAlg where

  coAlg :: Exp -> ExpF (Either Exp Exp)
  coAlg = \case
    SReturn (ConstTagNode _ [arg]) -> SReturnF (Var arg)
    EBind lhs bPat rhs -> EBindF (Left lhs) bPat (Right rhs)
    SBlock body -> SBlockF (Right body)
    e -> Left <$> project e

commonTag :: [TagInfo] -> TagInfo
commonTag (t : ts)
  | all (==t) ts = t
commonTag _ = Unknown
