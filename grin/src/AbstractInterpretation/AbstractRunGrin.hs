{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.AbstractRunGrin
  ( abstractRun
  ) where

import Debug.Trace

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.State
import Control.Monad.Reader
import Text.Printf

import qualified Data.Functor.Foldable as Foldable
import Control.Comonad.Cofree

import Grin
import AbstractInterpretation.HPTResult

type AExp = Cofree ExpF Int
type ASimpleExp = AExp
type ADef = AExp
type AProgram = AExp

type ADefMap = Map Name ADef

{-
  TODO:
    decide the subset of grin (e.g. high level grin) that HPT should operate on ; what language constructs should be supported?
    implement equasion solver for the specific example from the grin paper as a separate app
-}

type GrinM = ReaderT ADefMap (State Computer)

{-
bindPatMany :: Env -> [RTVal] -> [LPat] -> Env
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env (Set.singleton Undefined) lpat) [] lpats
bindPatMany _ vals lpats = error $ "bindPatMany - pattern mismatch: " ++ show (vals, lpats)
-}
bindPat :: VarSet -> LPat -> GrinM Bool
bindPat val lpat = case lpat of
  Var n -> addToEnv n val
{-
  ConstTagNode ptag pargs   | ConstTagNode vtag vargs <- val, ptag == vtag -> bindPatMany env vargs pargs
  VarTagNode varname pargs  | ConstTagNode vtag vargs <- val               -> bindPatMany (Map.insert varname (ValTag vtag) env) vargs pargs
-}
  ConstTagNode {} -> pure False -- TODO
  Unit -> pure False
  _ -> fail $ "ERROR: bindPat - pattern mismatch" ++ show (val,lpat)


addStep :: ASimpleExp -> GrinM ()
addStep exp = modify' (\computer@Computer{..} -> computer {steps = StepExp (stripBind $ unwrap exp) : steps}) where
  stripBind = \case
    EBindF op pat _ -> SApp "" [] -- EBind op pat (SApp "" [])
    e -> SApp (show e) []

addAssign :: Name -> VarSet -> GrinM ()
addAssign name val = modify' (\computer@Computer{..} -> computer {steps = StepAssign name val : steps}) where

addToEnv :: Name -> VarSet -> GrinM Bool -- False if nothing has changed
addToEnv name val = addAssign name val >> state updateEnv where
  -- TODO: log new additions
  updateEnv computer@Computer{..}
    | isSubset envMap = (False, computer)
    | otherwise       = (True, computer {envMap = Map.insertWith mappend name val envMap})

  isSubset envMap = case Map.lookup name envMap of
    Nothing -> False
    Just v  -> val `Set.isSubsetOf` v

-- TODO: log new additions
addToStore :: Int -> NodeSet -> GrinM ()
addToStore loc val = modify' (\computer@Computer{..} -> computer {storeMap = IntMap.insertWith mappend loc val storeMap})

lookupEnv :: Name -> GrinM VarSet
lookupEnv n = Map.findWithDefault (error $ "missing variable: " ++ n) n <$> gets envMap

lookupStore :: Int -> GrinM NodeSet
lookupStore i = IntMap.findWithDefault (error $ "missing location: " ++ show i) i <$> gets storeMap

basVarSet cgType = Set.singleton . V . BAS $ cgType

boolVarSet = Set.fromList
  [ N $ RTNode (Tag C "True") []
  , N $ RTNode (Tag C "False") []
  ]

toRTLocVal :: RTVar -> RTLocVal
toRTLocVal (V a) = a
toRTLocVal a = error $ "toRTLocVal: illegal value " ++ show a
{-
toRTNode :: RTVar -> RTNode
toRTNode (N a) = a
toRTNode a = error $ "toRTNode: illegal value " ++ show a
-}
evalVal :: Val -> GrinM VarSet
evalVal = \case
  v@Lit{}     -> pure $ basVarSet T_Int64
  Var n       -> lookupEnv n
  ConstTagNode t a -> Set.singleton . N . RTNode t <$> mapM (\x -> Set.map toRTLocVal <$> evalVal x) a
{-
  -- SKIP this now
  VarTagNode n a -> do
                  args <- mapM (\x -> Set.map toRTLocVal <$> evalVal x) a
                  values <- Set.toList <$> lookupEnv
                  -- TODO: support TagValue ; represent it as normal value instead of BAS
                  pure $ Set.fromList [N $ RTNode t args | t <- values]
-}
  v@ValTag{}  -> pure $ basVarSet T_Tag
  v@Unit      -> pure $ basVarSet T_Unit
  v@Loc{}     -> pure $ basVarSet T_Loc
  x -> fail $ "ERROR: evalVal: " ++ show x


selectRTNodeItem :: Maybe Int -> RTVar -> VarSet
selectRTNodeItem Nothing val = Set.singleton val
selectRTNodeItem (Just 0) (N (RTNode tag args)) = basVarSet T_Tag
selectRTNodeItem (Just i) (N (RTNode tag args)) = Set.map V $ (args !! (i - 1))

evalSFetchF :: Maybe Int -> VarSet -> GrinM VarSet
evalSFetchF index vals = mconcat <$> mapM fetch (Set.toList vals) where
  fetch = \case
    V (RTLoc l) -> {-Set.map N <$> -}mconcat . map (selectRTNodeItem index) . Set.toList <$> lookupStore l
    x -> fail $ "ERROR: evalSimpleExp - Fetch expected location, got: " ++ show x

evalSUpdateF :: VarSet-> NodeSet -> GrinM VarSet
evalSUpdateF vals v' = mapM_ update vals >> pure (basVarSet T_UNKNOWN) where
 update = \case
   V (RTLoc l) -> IntMap.member l <$> gets storeMap >>= \case
             False -> fail $ "ERROR: evalSimpleExp - Update unknown location: " ++ show l
             True  -> addToStore l v'
   x -> fail $ "ERROR: evalSimpleExp - Update expected location, got: " ++ show x

evalEval :: [Val] -> GrinM VarSet
evalEval [val] = do
  loc <- evalVal val
  nodes <- evalSFetchF Nothing loc
  {-
    NOTE:
      F nodes   - call the function
      otherwise - keep the value
  -}
  evalNodes <- forM (Set.toList nodes) $ \case
    N (RTNode (Tag F name) args) -> do
      result <- evalSAppF name (map (Set.map V) args)
      evalSUpdateF loc result
      pure result
    value -> pure $ Set.singleton value
  pure $ mconcat evalNodes

evalSAppF n rtVals = do
  _ :< (DefF _ vars body) <- reader $ Map.findWithDefault (error $ "unknown function: " ++ n) n
  unless (length vars == length rtVals) $ fail "ERROR: SApp"
  -- FIX
  new <- or <$> zipWithM bindPat rtVals (map Var vars)
  case new of
    False -> pure . Set.singleton . V $ RTVar n -- add placeholder TODO: include args
    True  -> do
      result <- evalExp body
      addToEnv n result
      -- TODO: remove placeholders which are subset in terms of args
      pure result

evalSimpleExp :: ASimpleExp -> GrinM VarSet
evalSimpleExp = \case

  _ :< (SAppF n args) -> do
              rtVals <- mapM evalVal args -- Question: is this correct here?
              case n of
                -- Special case
                -- "eval" -> evalEval args
                -- Primitives
                "_prim_int_print"  -> pure $ basVarSet $ T_Fun "_prim_int_print"
                "_prim_int_gt"    -> pure $ basVarSet $ T_Fun "_prim_int_gt" --boolVarSet
                "_prim_int_add"   -> pure $ basVarSet T_Int64
                -- User defined functions
                _ -> do
                  evalSAppF n rtVals

  _ :< (SReturnF v) -> evalVal v

  l :< (SStoreF v) -> do
              v' <- {-Set.map toRTNode <$> -}evalVal v
              addToStore l v'
              pure . Set.singleton . V $ RTLoc l

  _ :< (SFetchIF n i) -> lookupEnv n >>= evalSFetchF i

  _ :< (SUpdateF n v) -> do
              v' <- {-Set.map toRTNode <$> -}evalVal v
              vals <- lookupEnv n
              evalSUpdateF vals v'

  _ :< (SBlockF a) -> evalExp a

  x -> fail $ "ERROR: evalSimpleExp: " ++ show x


evalExp :: AExp -> GrinM VarSet
evalExp x = {-addStep x >> -}case x of
  _ :< (EBindF op pat exp) -> do
    evalSimpleExp op >>= \v -> bindPat v pat >> evalExp exp

  {-
    TODO:
      NEVER - evaluate a case if there was a new value in the pattern args (optimization)
      ANSWER: the assumtion is wrong, the case alt binders does not capture all used variables of the given branch
  -}
  _ :< (ECaseF v alts) -> evalVal v >>= \vals -> do
    a <- mconcat <$> sequence
      [ zipWithM_ addToEnv names (map (Set.map V) args) >> evalExp exp
      | N (RTNode tag args) <- Set.toList vals
      , AltF (NodePat alttag names) exp <- map unwrap alts
      , tag == alttag
      ]
    -- what is this???
    case [() | V (BAS _) <- Set.toList vals] of
      [] -> pure a
      _  -> do
        let notNodePat = \case
              NodePat{} -> False
              _ -> True
        b <- mconcat <$> sequence
          [ evalExp exp
          | AltF pat exp <- map unwrap alts
          , notNodePat pat
          ]
        pure $ mconcat [a, b]

  exp -> evalSimpleExp exp


abstractRun :: AProgram -> Name -> (VarSet, Computer)
abstractRun (_ :< ProgramF l) n = runState (runReaderT (evalExp e) m) emptyComputer where
  m = Map.fromList [(n,d) | d@(_ :< (DefF n _ _)) <- l]
  e = case Map.lookup n m of
        Nothing -> error $ "missing function: " ++ n
        Just (_ :< (DefF _ [] a)) -> a
        _ -> error $ "function " ++ n ++ " has arguments"
