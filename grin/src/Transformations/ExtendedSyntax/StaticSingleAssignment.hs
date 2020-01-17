{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Transformations.ExtendedSyntax.StaticSingleAssignment where

import Control.Arrow (first)
import Data.Functor.Foldable
import Data.Monoid hiding (Alt)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Lens.Micro.Platform ((%=), _1, over)

import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Util

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
      subst' <- foldM (\s n -> snd <$> calcName (n, s)) subst (foldNames (:[]) v)
      pure $ EBindF (subst', lhs) (mapNamesBPat (substName' subst') v) (subst', rhs)

    Alt (NodePat tag names) name body -> do
      (names0, subst0) <- first reverse <$> foldM
        (\(names1, subst1) name1 -> first (:names1) <$> calcName (name1, subst1))
        ([], subst)
        names
      -- QUESTION: is this correct?
      (name0, subst1) <- calcName (name, subst0)
      pure $ AltF (NodePat tag names0) name0 (subst1, body)

    -- QUESTION: is this correct?
    Alt cpat name body -> do
      (name0, subst0) <- calcName (name, subst)
      pure $ AltF cpat name0 (subst0, body)

    -- Substituitions
    ECase       scrut alts -> pure $ ECaseF (substName scrut) $ ((,) subst) <$> alts
    SApp        f params -> pure $ SAppF (substName f) (map substName params)
    SReturn     val -> pure $ SReturnF (substVal val)
    SStore      var -> pure $ SStoreF (substName var)
    SFetch      var -> pure $ SFetchF (substName var)
    SUpdate     ptr var -> pure $ SUpdateF (substName ptr) (substName var)

    rest -> pure $ (,) subst <$> project rest
    where
      -- đ Checks if the name is already registered, register it if necessary and
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

      -- | Create a new name by appending an index to an already existing one.
      newName :: Name -> Int -> Name
      newName nm i = nm <> "_" <> showTS i

      -- | Basically a mapping on `Name`s inside a `Val` using the
      -- `substName` function.
      substVal :: Val -> Val
      substVal = \case
        ConstTagNode  tag params -> ConstTagNode tag (map substName params)
        Var nm  -> Var $ substName nm
        rest    -> rest

      -- | Lookup a name using the context in the local closure.
      -- Produce a new name if the given name is already present.
      substName :: Name -> Name
      substName nm = maybe nm (newName nm) (Map.lookup nm subst)

      -- | Lookup a name using a given context.
      -- Produce a new name if the given name is already present.
      substName' :: Map.Map Name Int -> Name -> Name
      substName' s nm = maybe nm (newName nm) (Map.lookup nm s)
