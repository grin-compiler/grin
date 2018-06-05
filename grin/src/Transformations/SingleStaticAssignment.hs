{-# LANGUAGE LambdaCase #-}
module Transformations.SingleStaticAssignment where

import Grin
import Data.Functor.Foldable
import Data.Monoid hiding (Alt)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Transformations.Util
import Control.Monad.State

type VarM a = State (Set.Set Name, Int) a


singleStaticAssignment :: Exp -> Exp
singleStaticAssignment e = evalState (anaM build (mempty, e)) (mempty, 1) where
  build :: (Map.Map Name Int, Exp) -> VarM (ExpF (Map.Map Name Int, Exp))
  build (subst, e) = case e of
    EBind lhs v@(Var nm) rhs -> do
      (nm', subst') <- calcName (nm, subst)
      pure $ EBindF (subst', lhs) (Var nm') (subst', rhs)

    Alt (NodePat tag names) body -> do
      (names0, subst0) <- foldM
        (\(names1, subst1) name1 -> do
          (name2, subst2) <- calcName (name1, subst1)
          pure (name2:names1, subst2))
        ([], subst)
        names
      pure $ AltF (NodePat tag (reverse names0)) (subst0, body)

    -- Substituitions
    ECase       val alts -> pure $ ECaseF (substVal val) $ ((,) subst) <$> alts
    SApp        name params -> pure $ SAppF name (substVal <$> params)
    SReturn     val -> pure $ SReturnF (substVal val)
    SStore      val -> pure $ SStoreF (substVal val)
    SFetchI     name pos -> pure $ SFetchIF (substName name) pos
    SUpdate     name val -> pure $ SUpdateF (substName name) (substVal val)

    rest -> pure $ (,) subst <$> project rest
    where
      -- Checks if the name is already registered, register it if necessary and
      -- returns a unique name in the block.
      calcName :: (Name, Map.Map Name Int) -> VarM (Name, Map.Map Name Int)
      calcName (nm, subst) = do
        (vars, idx) <- get
        case (Set.member nm vars, Map.lookup nm subst) of
          (False, Nothing) -> do
            put (Set.insert nm vars, idx)
            pure (nm, subst)
          (True, _) -> do
            let nm' = newName nm idx
                subst' = Map.insert nm idx subst
            put (Set.insert nm vars, idx + 1)
            pure (nm', subst')

      newName nm i = concat [nm, "_", show i]

      substVal = \case
        ConstTagNode  tag params -> ConstTagNode tag (substVal <$> params)
        VarTagNode    nm  params -> VarTagNode (substName nm) (substVal <$> params)
        Var nm  -> Var $ substName nm
        rest    -> rest

      substName nm = maybe nm (newName nm) (Map.lookup nm subst)
