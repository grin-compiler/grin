{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractRunGrin (abstractRun, Computer(..)) where

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

import Grin

data RTLocVal
  = RTLoc Int
  | BAS
  deriving (Eq, Ord, Show)

data RTNode = RTNode Tag [Set RTLocVal]
  deriving (Eq, Ord, Show)

data RTVar
  = N RTNode
  | V RTLocVal
  deriving (Eq, Ord, Show)

type NodeSet = Set RTNode
type VarSet = Set RTVar -- HINT: VarVal in the paper

data Computer
  = Computer
  { storeMap  :: IntMap NodeSet   -- models the computer memory
  , envMap    :: Map Name VarSet  -- models the CPU registers
  , steps     :: [Exp]
  , counter   :: !Int
  }
  deriving Show

emptyComputer = Computer mempty mempty mempty 0

type GrinM = ReaderT Prog (State Computer)

{-
bindPatMany :: Env -> [RTVal] -> [LPat] -> Env
bindPatMany env [] [] = env
bindPatMany env (val : vals) (lpat : lpats) = bindPatMany (bindPat env val lpat) vals lpats
bindPatMany env [] (lpat : lpats) = bindPatMany (bindPat env (Set.singleton Undefined) lpat) [] lpats
bindPatMany _ vals lpats = error $ "bindPatMany - pattern mismatch: " ++ show (vals, lpats)
-}
bindPat :: VarSet -> LPat -> GrinM ()
bindPat val lpat = case lpat of
  Var n -> addToEnv n val
{-
  ConstTagNode ptag pargs   | ConstTagNode vtag vargs <- val, ptag == vtag -> bindPatMany env vargs pargs
  VarTagNode varname pargs  | ConstTagNode vtag vargs <- val               -> bindPatMany (Map.insert varname (ValTag vtag) env) vargs pargs
-}
  Unit -> pure ()
  _ -> fail $ "ERROR: bindPat - pattern mismatch" ++ show (val,lpat)

addStep :: SimpleExp -> GrinM ()
addStep exp = modify' (\computer@Computer{..} -> computer {steps = exp : steps, counter = succ counter})

addToEnv :: Name -> VarSet -> GrinM ()
addToEnv name val = modify' (\computer@Computer{..} -> computer {envMap = Map.insertWith mappend name val envMap})

lookupEnv :: Name -> GrinM VarSet
lookupEnv n = Map.findWithDefault (error $ "missing variable: " ++ n) n <$> gets envMap

lookupStore :: Int -> GrinM NodeSet
lookupStore i = IntMap.findWithDefault (error $ "missing location: " ++ show i) i <$> gets storeMap

basVarSet = Set.singleton $ V BAS

toRTLocVal :: RTVar -> RTLocVal
toRTLocVal (V a) = a
toRTLocVal a = error $ "toRTLocVal: illegal value " ++ show a

toRTNode :: RTVar -> RTNode
toRTNode (N a) = a
toRTNode a = error $ "toRTNode: illegal value " ++ show a

evalVal :: Val -> GrinM VarSet
evalVal = \case
  v@Lit{}     -> pure basVarSet
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
  v@ValTag{}  -> pure basVarSet
  v@Unit      -> pure basVarSet
  v@Loc{}     -> pure basVarSet
  x -> fail $ "ERROR: evalVal: " ++ show x


evalSimpleExp :: SimpleExp -> GrinM VarSet
evalSimpleExp = \case

  -- TODO: use bind
  SApp n args -> case n of
                "add" -> pure basVarSet
                "mul" -> pure basVarSet
                "intPrint" -> pure basVarSet
                "intGT" -> pure basVarSet
                "intAdd" -> pure basVarSet
                _ -> do
                  Def _ vars body <- reader $ Map.findWithDefault (error $ "unknown function: " ++ n) n
                  unless (length vars == length args) $ fail "ERROR: SApp"
                  rtVals <- mapM evalVal args -- Question: is this correct here?
                  zipWithM_ bindPat rtVals (map Var vars)
                  result <- evalExp body
                  addToEnv n result
                  pure result

  SReturn v -> evalVal v

  SStore v -> do
              let l = 0 -- TODO: make it constant for each store AST operation
              v' <- Set.map toRTNode <$> evalVal v
              modify' (\computer@Computer{..} -> computer {storeMap = IntMap.insertWith mappend l v' storeMap})
              pure . Set.singleton . V $ RTLoc l

  SFetch n -> lookupEnv n >>= \vals -> mconcat <$> mapM fetch (Set.toList vals) where
                fetch = \case
                  V (RTLoc l) -> Set.map N <$> lookupStore l
                  x -> fail $ "ERROR: evalSimpleExp - Fetch expected location, got: " ++ show x

  SUpdate n v -> do
              v' <- Set.map toRTNode <$> evalVal v
              let update = \case
                    V (RTLoc l) -> IntMap.member l <$> gets storeMap >>= \case
                              False -> fail $ "ERROR: evalSimpleExp - Update unknown location: " ++ show l
                              True  -> modify' (\computer@Computer{..} -> computer {storeMap = IntMap.insertWith mappend l v' storeMap})
                    x -> fail $ "ERROR: evalSimpleExp - Update expected location, got: " ++ show x
              lookupEnv n >>= \vals -> mapM_ update vals >> pure basVarSet

  SBlock a -> evalExp a

  x -> fail $ "ERROR: evalSimpleExp: " ++ show x


evalExp :: Exp -> GrinM VarSet
evalExp x = addStep x >> case x of
  EBind op pat exp -> do
    cnt <- gets counter
    case cnt < 10 of
      True  -> evalSimpleExp op >>= \v -> bindPat v pat >> evalExp exp
      False -> pure basVarSet -- TERMINATE

  ECase v alts -> evalVal v >>= \vals -> do
    a <- mconcat <$> sequence [zipWithM_ addToEnv names (map (Set.map V) args) >> evalExp exp | N (RTNode tag args) <- Set.toList vals, Alt (NodePat alttag names) exp <- alts, tag == alttag]
    pure a
{-
    ConstTagNode t l ->
                   let (vars,exp) = head $ [(b,exp) | Alt (NodePat a b) exp <- alts, a == t] ++ error ("evalExp - missing Case Node alternative for: " ++ show t)
                       go a [] [] = a
                       go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
                       go _ x y = error $ "invalid pattern and constructor: " ++ show (t,x,y)
                   in  evalExp (go env vars l) exp
    ValTag t    -> evalExp env $ head $ [exp | Alt (TagPat a) exp <- alts, a == t] ++ error ("evalExp - missing Case Tag alternative for: " ++ show t)
    Lit l       -> evalExp env $ head $ [exp | Alt (LitPat a) exp <- alts, a == l] ++ error ("evalExp - missing Case Lit alternative for: " ++ show l)
    x -> error $ "evalExp - invalid Case dispatch value: " ++ show x
-}
  exp -> evalSimpleExp exp

abstractRun :: [Def] -> Name -> (VarSet, Computer)
abstractRun l n = runState (runReaderT (evalExp e) m) emptyComputer where
  m = Map.fromList [(n,d) | d@(Def n _ _) <- l]
  e = case Map.lookup n m of
        Nothing -> error $ "missing function: " ++ n
        Just (Def _ [] a) -> a
        _ -> error $ "function " ++ n ++ " has arguments"
