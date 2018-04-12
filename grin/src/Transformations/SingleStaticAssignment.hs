{-# LANGUAGE LambdaCase #-}
module Transformations.SingleStaticAssignment where

import Grin
import Data.Functor.Foldable
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import Transformations.Util
import Control.Monad.State

type VarM a = State (Set.Set Name, Int) a


singleStaticAssignment :: Exp -> Exp
singleStaticAssignment e = fst $ runState (anaM build (mempty, e)) (mempty, 1) where
  build :: (Map.Map Name Int, Exp) -> VarM (ExpF (Map.Map Name Int, Exp))
  build (subst, e) = case e of
    EBind lhs v@(Var nm) rhs -> do
      (vars, idx) <- get
      case (Set.member nm vars, Map.lookup nm subst) of
        (False, Nothing) -> do
          put (Set.insert nm vars, idx)
          pure $ EBindF (subst, lhs) v (subst, rhs)
        (True, Nothing) -> do
          let nm' = newName nm idx
              subst' = Map.insert nm idx subst
          put (Set.insert nm vars, idx + 1)
          pure $ EBindF (subst', lhs) (Var nm') (subst', rhs)
        (True, Just _) -> do
          let nm' = newName nm idx
              subst' = Map.insert nm idx subst
          put (Set.insert nm vars, idx + 1)
          pure $ EBindF (subst', lhs) (Var nm') (subst', rhs)

    -- Substituitions
    ECase       val alts -> pure $ ECaseF (substVal val) (((,) subst) <$> alts)
    SApp        name params -> pure $ SAppF name (substVal <$> params)
    SReturn     val -> pure $ SReturnF (substVal val)
    SStore      val -> pure $ SStoreF (substVal val)
    SFetchI     name pos -> pure $ SFetchIF (substName name) pos
    SUpdate     name val -> pure $ SUpdateF (substName name) (substVal val)

    rest -> pure $ ((,) subst) <$> project rest
    where
      newName nm i = concat [nm, "_", show i]

      substVal = \case
        ConstTagNode  tag params -> ConstTagNode tag (substVal <$> params)
        VarTagNode    nm  params -> VarTagNode (substName nm) (substVal <$> params)
        Var nm  -> Var $ substName nm
        rest    -> rest

      substName nm = maybe nm (newName nm) (Map.lookup nm subst)
