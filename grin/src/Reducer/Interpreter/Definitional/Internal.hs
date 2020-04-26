{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, EmptyCase, RankNTypes #-}
module Reducer.Interpreter.Definitional.Internal where

import Control.Monad (forM_, when)
import Control.Monad.Fail
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (MonadIO(liftIO), lift)
import Control.Monad.Trans.Reader hiding (ask, local)
import Control.Monad.Trans.State hiding (state, get)
import Data.Either (fromLeft)
import Data.Int
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Word
import Data.Text (Text)
import Grin.ExtendedSyntax.Syntax (Name(..), Exp(..), Tag(..), Lit(..))
import qualified Grin.ExtendedSyntax.Syntax as Syntax
import Reducer.Interpreter.Base
import Lens.Micro.Platform
import Prelude hiding (fail)

import Reducer.Interpreter.Store (Store(..))
import qualified Reducer.Interpreter.Store as Store
import Reducer.Interpreter.Env (Env)
import qualified Reducer.Interpreter.Env as Env
import qualified Data.Map.Strict as Map
import qualified Grin.Syntax as SyntaxV1 (Exp, Name(..), Tag(..), Lit(..))
import Reducer.Base (RTVal(..), Statistics(..))
import Reducer.Pure (EvalPlugin(..))
import Grin.Statistics (Statistics(..))
import Transformations.ExtendedSyntax.Conversion (convertToNew, convert)
import Data.Functor.Foldable
import Data.Functor.Infix ((<$$>))


data SVal
  = SInt64  Int64
  | SWord64 Word64
  | SFloat  Float
  | SBool   Bool
  | SChar   Char
  | SString Text
  | SLoc    Loc
  deriving (Eq, Ord, Show)

data Node = Node Tag [SVal]
  deriving (Eq, Ord, Show)

newtype Loc = Loc Int
  deriving (Eq, Ord, Show)

data DVal
  = DNode Node
  | DVal  SVal
  | DUnit
  deriving (Eq, Ord, Show)

data DefEnv m e v = DefEnv
  { _defFuns :: Map.Map Name (Fix (Syntax.ExpF :+: e))
  , _defOps  :: Map.Map Name ([v] -> m v)
  , _defEnv  :: Env v
  }

makeLenses ''DefEnv

data RefStats = RefStats { fetched :: !Int, update :: !Int }

data HeapNode i = HeapNode { heapNode :: !(Maybe Node) , info :: !i }
  deriving Show

class HeapInfo i where
  storeHeapInfo  :: i
  fetchHeapInfo  :: i -> i
  updateHeapInfo :: i -> i

data NoHeapInfo = NoHeapInfo deriving Show
instance HeapInfo NoHeapInfo where
  storeHeapInfo    = NoHeapInfo
  fetchHeapInfo  x = x
  updateHeapInfo x = x

instance HeapInfo RefStats where
  storeHeapInfo                   = RefStats 0 0
  fetchHeapInfo   (RefStats f u)  = RefStats (succ f) u
  updateHeapInfo  (RefStats f u)  = RefStats f (succ u)

-- TODO: Use RWST
newtype DefinitionalT (m :: * -> *) (i :: *) (e :: * -> *) (v :: *) (a :: *) = DefinitionalT
  { definitionalT :: StateT (Store Loc (HeapNode i)) (ReaderT (DefEnv m e (Either DVal v)) m) a
  } deriving
      ( Functor
      , Applicative
      , Monad
      , MonadFail
      , MonadIO
      , MonadReader (DefEnv m e (Either DVal v))
      , MonadState (Store Loc (HeapNode i))
      )

lookupFun :: (Monad m) => Name -> (DefinitionalT m i e v) (Fix (Syntax.ExpF :+: e))
lookupFun funName = (fromMaybe (error $ "Missing:" ++ show funName) . Map.lookup funName . _defFuns) <$> ask

data DefinitionalTContext (e :: * -> *) (v :: *) (i :: *) = DefinitionalTContext

runDefinitionalT
  :: (Monad m)
  => DefinitionalTContext e v i
  -> Map.Map Syntax.Name ([DVal] -> m DVal)
  -> (Fix (Syntax.ExpF :+: e))
  -> DefinitionalT m i e v a
  -> m (a, (Store Loc (HeapNode i)))
runDefinitionalT _ ops prog n = runReaderT (runStateT (definitionalT n) Store.empty) env
  where
    ops' = Map.map (\f xs -> fmap Left $ f (map (fromLeft (error "fromLeft #1")) xs)) ops
    env = DefEnv (programToDefs prog) ops' Env.empty
