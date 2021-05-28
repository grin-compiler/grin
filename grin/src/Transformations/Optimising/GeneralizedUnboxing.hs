{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Transformations.Optimising.GeneralizedUnboxing where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.Function
import Data.Functor.Foldable as Foldable
import Data.Functor.Infix
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Applicative
import Lens.Micro.Platform
import Control.Monad.Writer
import Control.Arrow
import Debug.Trace

import Transformations.Util (anaM, apoM)
import Transformations.Names

import Grin.Grin
import Grin.TypeEnv
import Grin.Pretty


generalizedUnboxing :: TypeEnv -> Exp -> (Exp, ExpChanges)
generalizedUnboxing te exp = if (null funs)
  then (exp, NoChange)
  else second
        (const NewNames) -- New functions are created, but NameM monad is not used
        (evalNameM exp (transformCalls funs te =<< transformReturns funs te exp))
  where
    funs = functionsToUnbox te exp

-- TODO: Support tagless nodes.

tailCalls :: Exp -> Maybe [Name]
tailCalls = cata collect where
  collect :: ExpF (Maybe [Name]) -> Maybe [Name]
  collect = \case
    DefF _ _ result   -> result
    EBindF _ _ result -> result
    ECaseF _ alts -> nonEmpty $ concat $ catMaybes alts
    AltF _ result -> result
    SAppF f _     -> Just [f]
    e -> Nothing

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

doesReturnAKnownProduct :: TypeEnv -> Name -> Bool
doesReturnAKnownProduct = isJust <$$> returnsAUniqueTag

returnsAUniqueTag :: TypeEnv -> Name -> Maybe (Tag, Type)
returnsAUniqueTag te name = do
  (tag, vs) <- te ^? function . at name . _Just . _1 . _T_NodeSet . to Map.toList . to singleton . _Just
  typ       <- singleton (Vector.toList vs)
  pure (tag, T_SimpleType typ)

singleton :: [a] -> Maybe a
singleton = \case
  []  -> Nothing
  [a] -> Just a
  _   -> Nothing

transitive :: (Ord a) => (a -> Set a) -> Set a -> Set a
transitive f res0 =
  let res1 = res0 `Set.union` (Set.unions $ map f $ Set.toList res0)
  in if res1 == res0
      then res0
      else transitive f res1

-- TODO: Remove the fix combinator, explore the function
-- dependency graph and rewrite disqualify steps based on that.
functionsToUnbox :: TypeEnv -> Exp -> Set Name
functionsToUnbox te (Program exts defs) = result where
  funName (Def n _ _) = n

  tailCallsMap :: Map Name [Name]
  tailCallsMap = Map.fromList $ mapMaybe (\e -> (,) (funName e) <$> tailCalls e) defs

  tranisitiveTailCalls :: Map Name (Set Name)
  tranisitiveTailCalls = Map.fromList $ map (\k -> (k, transitive inTailCalls (Set.singleton k))) $ Map.keys tailCallsMap
    where
      inTailCalls :: Name -> Set Name
      inTailCalls n = maybe mempty Set.fromList $ Map.lookup n tailCallsMap

  nonCandidateTailCallMap = Map.withoutKeys tranisitiveTailCalls result0
  candidateCalledByNonCandidate = (Set.unions $ Map.elems nonCandidateTailCallMap) `Set.intersection` result0
  result = Set.delete "grinMain" $ result0 `Set.difference` candidateCalledByNonCandidate

  result0 = Set.fromList $ step initial
  initial = map funName $ filter (doesReturnAKnownProduct te . funName) defs
  disqualify candidates = filter
    (\candidate -> case Map.lookup candidate tailCallsMap of
      Nothing    -> True
      Just calls -> all (`elem` candidates) calls)
    candidates
  step = fix $ \rec x0 ->
    let x1 = disqualify x0 in
    if x0 == x1
      then x0
      else rec x1

updateTypeEnv :: Set Name -> TypeEnv -> TypeEnv
updateTypeEnv funs te = te & function %~ unboxFun
  where
    unboxFun = Map.fromList . map changeFun . Map.toList
    changeFun (n, ts@(ret, params)) =
      if Set.member n funs
        then (,) (n <> ".unboxed")
          $ maybe ts ((\t -> (t, params)) . T_SimpleType) $
              ret ^? _T_NodeSet
                  . to Map.elems
                  . to singleton
                  . _Just
                  . to Vector.toList
                  . to singleton
                  . _Just
        else (n, ts)

transformReturns :: Set Name -> TypeEnv -> Exp -> NameM Exp
transformReturns toUnbox te exp = apoM builder (Nothing, exp) where
  builder :: (Maybe (Tag, Type), Exp) -> NameM (ExpF (Either Exp (Maybe (Tag, Type), Exp)))
  builder (mTagType, exp0) = case exp0 of
    Def name params body
      | Set.member name toUnbox -> pure $ DefF name params (Right (returnsAUniqueTag te name, body))
      | otherwise               -> pure $ DefF name params (Left body)

    -- Always skip the lhs of a bind.
    EBind lhs pat rhs -> pure $ EBindF (Left lhs) pat (Right (mTagType, rhs))

    -- Remove the tag from the value
    SReturn (ConstTagNode tag [val]) -> pure $ SReturnF val

    -- Rewrite a node variable
    simpleExp
      -- fromJust works, as when we enter the processing of body of the
      -- expression only happens with the provided tag.
      | canUnbox simpleExp
      , Just (tag, typ) <- mTagType
      -> do
        freshName <- deriveNewName $ "unboxed." <> (showTS $ PP tag)
        pure . SBlockF . Left $ EBind simpleExp (ConstTagNode tag [Var freshName]) (SReturn $ Var freshName)

    rest -> pure (Right . (,) mTagType <$> project rest)

  -- NOTE: SApp is handled by transformCalls
  canUnbox :: SimpleExp -> Bool
  canUnbox = \case
    SApp n ps -> n `Set.notMember` toUnbox
    SReturn{} -> True
    SFetchI{} -> True
    _         -> False

transformCalls :: Set Name -> TypeEnv -> Exp -> NameM Exp
transformCalls toUnbox typeEnv exp = anaM builderM (True, Nothing, exp) where
  builderM :: (Bool, Maybe Name, Exp) -> NameM (ExpF (Bool, Maybe Name, Exp))

  builderM (isRightExp, mDefName, e) = case e of

    Def name params body
      -> pure $ DefF (if Set.member name toUnbox then name <> ".unboxed" else name) params (True, Just name, body)

    -- track the control flow
    EBind lhs pat rhs -> pure $ EBindF (False, mDefName, lhs) pat (isRightExp, mDefName, rhs)

    SApp name params
      | Set.member name toUnbox
      , Just defName <- mDefName
      , unboxedName <- name <> ".unboxed"
      , Just (tag, fstType) <- returnsAUniqueTag typeEnv name
      -> if Set.member defName toUnbox && isRightExp

          -- from candidate to candidate: tailcalls do not need a transform
          then pure $ SAppF unboxedName params

          -- from outside to candidate
          else do
            freshName <- deriveNewName $ "unboxed." <> (showTS $ PP tag)
            pure . SBlockF . (isRightExp, mDefName,) $
              EBind (SApp unboxedName params) (Var freshName) (SReturn $ ConstTagNode tag [Var freshName])

    rest -> pure ((isRightExp, mDefName,) <$> project rest)
