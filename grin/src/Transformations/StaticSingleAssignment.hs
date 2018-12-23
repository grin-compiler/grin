{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Transformations.StaticSingleAssignment where

import Control.Arrow (first)
import Data.Functor.Foldable
import Data.Monoid hiding (Alt)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Lens.Micro.Platform ((%=), _1)

import Grin.Grin
import Transformations.Util

type VarM a = State (Set.Set Name, Int) a


staticSingleAssignment :: Exp -> Exp
staticSingleAssignment e = flip evalState (mempty, 1) $
  do cata functionNames e
     anaM build (mempty, e)
  where
  functionNames :: ExpF (VarM ()) -> VarM ()
  functionNames = \case
    ProgramF exts defs -> sequence_ defs
    DefF name ps body -> do
      _1 %= Set.insert name
      body
    rest -> pure ()

  build :: (Map.Map Name Int, Exp) -> VarM (ExpF (Map.Map Name Int, Exp))
  build (subst, e) = case e of

    Def name names body -> do
      (names0, subst0) <- first reverse <$> foldM
        (\(names1, subst1) name1 -> first (:names1) <$> calcName (name1, subst1))
        ([], subst)
        names
      pure $ DefF name names0 (subst0, body)

    EBind lhs v rhs -> do
      subst' <- foldM (\s n -> snd <$> calcName (n, s)) subst (foldNamesVal (:[]) v)
      pure $ EBindF (subst', lhs) (mapNamesVal (substName' subst') v) (subst', rhs)

    Alt (NodePat tag names) body -> do
      (names0, subst0) <- first reverse <$> foldM
        (\(names1, subst1) name1 -> first (:names1) <$> calcName (name1, subst1))
        ([], subst)
        names
      pure $ AltF (NodePat tag names0) (subst0, body)

    -- Substituitions
    ECase       val alts -> pure $ ECaseF (substVal val) $ ((,) subst) <$> alts
    SApp        name params -> pure $ SAppF (substName name) (substVal <$> params)
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

      newName nm i = nm <> "_" <> showTS i

      substVal = \case
        ConstTagNode  tag params -> ConstTagNode tag (substVal <$> params)
        VarTagNode    nm  params -> VarTagNode (substName nm) (substVal <$> params)
        Var nm  -> Var $ substName nm
        rest    -> rest

      substName nm = maybe nm (newName nm) (Map.lookup nm subst)
      substName' s nm = maybe nm (newName nm) (Map.lookup nm s)
