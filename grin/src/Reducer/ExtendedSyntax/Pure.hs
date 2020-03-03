{-# LANGUAGE LambdaCase, TupleSections, BangPatterns, OverloadedStrings #-}
module Reducer.ExtendedSyntax.Pure
  ( EvalPlugin(..)
  , reduceFun
  , reduceFunWithoutStats
  ) where

import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Foldable (fold)
import Control.Monad.State
import Control.Monad.Reader
import Text.Printf

import Reducer.ExtendedSyntax.Base
import Reducer.ExtendedSyntax.PrimOps
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty

prettyDebug :: Pretty a => a -> String
prettyDebug = show . plain . pretty

-- models computer memory
data StoreMap
  = StoreMap
  { storeMap  :: !(IntMap RTVal)
  , storeSize :: !Int
  }

emptyStore = StoreMap mempty 0

newtype EvalPlugin = EvalPlugin
  { evalPluginPrimOp  :: Name -> [Name] -> [RTVal] -> IO RTVal
  }
type Prog = Map Name Def
data Context = Context
  { ctxProg       :: Prog
  , ctxExternals  :: [External]
  , ctxEvalPlugin :: EvalPlugin
  }
type GrinM a = ReaderT Context (StateT (StoreMap, Statistics) IO) a

lookupStore :: Int -> StoreMap -> RTVal
lookupStore i s = IntMap.findWithDefault (error $ printf "missing location: %d" i) i $ storeMap s

debug :: Bool
debug = False

evalSimpleExp :: Env -> SimpleExp -> GrinM RTVal
evalSimpleExp env s = do
  when debug $ do
    liftIO $ print s
    void $ liftIO $ getLine
  case s of
    SApp fun args -> do
      let rtValArgs = map (evalVar env) args

          go a [] [] = a
          go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
          go _ x y = error $ printf "invalid pattern for function: %s %s %s" fun (prettyDebug x) (prettyDebug y)
      exts <- asks ctxExternals
      evalPrimOp <- asks (evalPluginPrimOp . ctxEvalPlugin)
      if isExternalName exts fun
        then liftIO $ evalPrimOp fun args rtValArgs
        else do
          Def _ vars body <- reader
            ((Map.findWithDefault (error $ printf "unknown function: %s" fun) fun) . ctxProg)
          evalExp (go env vars rtValArgs) body
    SReturn v -> pure $ evalVal env v
    SStore v -> do
      l <- gets (storeSize . fst)
      let v' = evalVar env v
      modify' $ \(StoreMap m s, Statistics f u) ->
        ( StoreMap (IntMap.insert l v' m) (s+1)
        , Statistics (IntMap.insert l 0 f) (IntMap.insert l 0 u)
        )
      pure $ RT_Loc l
    SFetch ptr -> case evalVar env ptr of
      RT_Loc l -> do
        modify' $ \(heap, Statistics f u) ->
          (heap, Statistics (IntMap.adjust (+1) l f) u)
        gets $ lookupStore l . fst
      x -> error $ printf "evalSimpleExp - Fetch expected location, got: %s" (prettyDebug x)
    SUpdate n v -> do
      let v' = evalVar env v
      case evalVar env n of
        RT_Loc l -> do
          StoreMap m _ <- fst <$> get
          case IntMap.member l m of
            False -> error $ printf "evalSimpleExp - Update unknown location: %d" l
            True  -> do
              modify' $ \(StoreMap m s, Statistics f u) ->
                ( StoreMap (IntMap.insert l v' m) s
                , Statistics f (IntMap.adjust (+1) l u)
                )
              pure RT_Unit
        x -> error $ printf "evalSimpleExp - Update expected location, got: %s" (prettyDebug x)
    SBlock a -> evalExp env a

    e@ECase{} -> evalExp env e -- FIXME: this should not be here!!! please investigate.

    x -> error $ printf "invalid simple expression %s" (prettyDebug x)

evalExp :: Env -> Exp -> GrinM RTVal
evalExp env = \case
  EBind op pat exp -> do
    v <- evalSimpleExp env op
    when debug $ do
      liftIO $ putStrLn $ unwords [show pat,":=",show v]
    evalExp (bindPat env v pat) exp
  ECase scrut alts -> do
    let defaultAlts = [exp | Alt DefaultPat _ exp <- alts]
        defaultAlt  = if length defaultAlts > 1
                        then error "multiple default case alternative"
                        else take 1 defaultAlts

        altNames      = [ name | Alt _ name _ <- alts ]
        scrutVal      = evalVar env scrut
        boundAltNames = fold $ map (`Map.singleton` scrutVal) altNames
        env'          = boundAltNames <> env
    case scrutVal of
      RT_ConstTagNode t l ->
        let (vars,exp) = head $ [(b,exp) | Alt (NodePat a b) _ exp <- alts, a == t] ++ map ([],) defaultAlt ++ error (printf "evalExp - missing Case Node alternative for: %s" (prettyDebug t))
            go a [] [] = a
            go a (x:xs) (y:ys) = go (Map.insert x y a) xs ys
            go _ x y = error $ printf "invalid pattern and constructor: %s %s %s" (prettyDebug t) (prettyDebug x) (prettyDebug y)
        in  evalExp
              (case vars of -- TODO: Better error check: If not default then parameters must match
                [] -> {-default-} env'
                _  -> go env' vars l)
              exp
      RT_Lit l -> evalExp env' $ head $ [exp | Alt (LitPat a) _ exp <- alts, a == l] ++ defaultAlt ++ error (printf "evalExp - missing Case Lit alternative for: %s" (prettyDebug l))
      x -> error $ printf "evalExp - invalid Case dispatch value: %s" (prettyDebug x)
  exp -> evalSimpleExp env exp

reduceFun :: EvalPlugin -> Program -> Name -> IO (RTVal, Maybe Statistics)
reduceFun evalPrimOp (Program exts l) n = do
  (v, (_, s)) <- runStateT (runReaderT (evalExp mempty e) context) (emptyStore, emptyStatistics)
  pure (v, Just s)
  where
    context@(Context m _ _) = Context (Map.fromList [(n,d) | d@(Def n _ _) <- l]) exts evalPrimOp
    e = case Map.lookup n m of
          Nothing -> error $ printf "missing function: %s" n
          Just (Def _ [] a) -> a
          _ -> error $ printf "function %s has arguments" n

reduceFunWithoutStats :: EvalPlugin -> Program -> Name -> IO RTVal
reduceFunWithoutStats evalPrimOp prog funName = fst <$> reduceFun evalPrimOp prog funName
