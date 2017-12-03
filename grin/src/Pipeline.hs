{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, PatternSynonyms #-}
module Pipeline where

import Control.Monad
import Text.Printf
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Eval
import Grin
import Pretty()
import Transformations
import TrafoPlayground
import AbstractRunGrin
import qualified CodeGenLLVM as CGLLVM
import qualified JITLLVM
import System.Process

import Data.Map as Map
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as Text

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Lens.Micro.TH
import Lens.Micro.Mtl
import Data.Set


type RenameVariablesMap = Map String String

data Transformation
  = CaseSimplification
  | SplitFetch
  | Vectorisation
  | RegisterIntroduction
  | BindNormalisation
  | RightHoistFetch
  | GenerateEval
  | RenameVariables RenameVariablesMap
  deriving (Eq, Ord, Show)

transformation :: HPTResult -> Int -> Transformation -> Exp -> Exp
transformation hptResult n = \case
  CaseSimplification      -> caseSimplification
  SplitFetch              -> splitFetch
  Vectorisation           -> vectorisation hptResult
  RegisterIntroduction    -> registerIntroductionI n
  BindNormalisation       -> bindNormalisation
  RightHoistFetch         -> rightHoistFetch
  GenerateEval            -> generateEval
  RenameVariables renames -> renameVaribales renames

newtype Hidden a = H a

instance Show (Hidden a) where
  show _ = "(hidden)"

instance Eq (Hidden a) where
  _ == _ = True

data Pipeline
  = HPT
  | T Transformation
  | TagInfo
  | PrintGrinH (Hidden (Doc -> Doc))
  | PureEval
  | JITLLVM
  | SaveLLVM FilePath
  | SaveGrin FilePath
  deriving Show

pattern PrintGrin :: (Doc -> Doc) -> Pipeline
pattern PrintGrin c <- PrintGrinH (H c)
  where PrintGrin c =  PrintGrinH (H c)

type TagInfo = Set Tag
type PipelineM a = StateT PState IO a
data PState = PState
    { _psExp :: Exp
    , _psTransStep :: Int
    , _psHPTResult :: Maybe HPTResult
    , _psTagInfo   :: Maybe TagInfo
    }

makeLenses ''PState

pipelineStep :: Pipeline -> PipelineM ()
pipelineStep = \case
  HPT           -> hpt
  T t           -> transformationM t
  TagInfo       -> tagInfo
  PrintGrin d   -> printGrinM d
  PureEval      -> pureEval
  JITLLVM       -> jitLLVM
  SaveLLVM path -> saveLLVM path
  SaveGrin path -> saveGrin path

hpt :: PipelineM ()
hpt = do
  grin <- use psExp
  let (_, result) = abstractRun (assignStoreIDs grin) "grinMain"
  liftIO $ print result
  psHPTResult .= Just result

transformationM :: Transformation -> PipelineM ()
transformationM t = do
  Just result <- use psHPTResult
  n           <- use psTransStep
  psExp       %= transformation result n t
  psTransStep %= (+1)

tagInfo :: PipelineM ()
tagInfo = do
  e <- use psExp
  psTagInfo .= Just (collectTagInfo e)

pureEval :: PipelineM ()
pureEval = do
  e <- use psExp
  liftIO . print . pretty $ evalProgram PureReducer e

printGrinM :: (Doc -> Doc) -> PipelineM ()
printGrinM color = do
  e <- use psExp
  liftIO . putStrLn . show . color $ pretty e

jitLLVM :: PipelineM ()
jitLLVM = do
  e <- use psExp
  liftIO $ do
    val <- JITLLVM.eagerJit (CGLLVM.codeGen e) "grinMain"
    print $ pretty val

saveGrin :: FilePath -> PipelineM ()
saveGrin fn = do
  n <- use psTransStep
  e <- use psExp
  liftIO . writeFile (concat [fn,".", show n]) . show $ pretty e

saveLLVM :: FilePath -> PipelineM ()
saveLLVM fname' = do
  e <- use psExp
  n <- use psTransStep
  let fname = concat [fname',".",show n]
      code = CGLLVM.codeGen e
      llName = printf "%s.ll" fname
      sName = printf "%s.s" fname
  liftIO . void $ do
    Text.putStrLn $ ppllvm code
    putStrLn "* to LLVM *"
    _ <- CGLLVM.toLLVM llName code
    putStrLn "* LLVM X64 codegen *"
    callProcess "llc-5.0" [llName]
    readFile sName >>= putStrLn

pipeline :: Exp -> [Pipeline] -> IO ()
pipeline e p = do
  print p
  flip evalStateT start . sequence_ $ Prelude.map pipelineStep p
  where
    start = PState e 0 Nothing Nothing
