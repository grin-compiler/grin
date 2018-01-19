{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, PatternSynonyms #-}
module Pipeline where

import Control.Monad
import Data.List (intersperse)
import Text.Printf
import Text.Pretty.Simple (pPrint)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Check
import Eval
import Grin
import Optimizations
import Pretty()
import Transformations.AssignStoreIDs
import Transformations.GenerateEval
import Transformations.Vectorisation
import Transformations.BindNormalisation
import Transformations.SplitFetch
import Transformations.CaseSimplification
import Transformations.RightHoistFetch
import Transformations.RegisterIntroduction
import Transformations.Playground
import AbstractInterpretation.AbstractRunGrin
import qualified Reducer.LLVM.CodeGen as CGLLVM
import qualified Reducer.LLVM.JIT as JITLLVM
import System.Directory
import System.Process

import Data.Map as Map
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as Text

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Lens.Micro.TH
import Lens.Micro.Mtl
import Data.Set
import System.FilePath


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
  -- Optimizations
  | ConstantFolding
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
  ConstantFolding         -> constantFolding

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
  | PrintAST
  | SaveLLVM FilePath
  | SaveGrin FilePath
  | DebugTransformationH (Hidden (Exp -> Exp))
  deriving Show

pattern PrintGrin :: (Doc -> Doc) -> Pipeline
pattern PrintGrin c <- PrintGrinH (H c)
  where PrintGrin c =  PrintGrinH (H c)

pattern DebugTransformation :: (Exp -> Exp) -> Pipeline
pattern DebugTransformation t <- DebugTransformationH (H t)
  where DebugTransformation t =  DebugTransformationH (H t)

data PipelineOpts = PipelineOpts
  { _poOutputDir :: FilePath
  }

type TagInfo = Set Tag
type PipelineM a = ReaderT PipelineOpts (StateT PState IO) a
data PState = PState
    { _psExp :: Exp
    , _psTransStep :: Int
    , _psHPTResult :: Maybe HPTResult
    , _psTagInfo   :: Maybe TagInfo
    }

makeLenses ''PState
makeLenses ''PipelineOpts

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
  PrintAST      -> printAST
  DebugTransformation t -> debugTransformation t

hpt :: PipelineM ()
hpt = do
  grin <- use psExp
  let (_, result) = abstractRun (assignStoreIDs grin) "grinMain"
--  liftIO $ print result
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
  Just hptResult <- use psHPTResult
  liftIO $ do
    val <- JITLLVM.eagerJit (CGLLVM.codeGen hptResult e) "grinMain"
    print $ pretty val

printAST :: PipelineM ()
printAST = do
  e <- use psExp
  pPrint e

saveGrin :: FilePath -> PipelineM ()
saveGrin fn = do
  n <- use psTransStep
  e <- use psExp
  outputDir <- view poOutputDir
  let fname = (concat [fn,".", show n])
  let content = show $ pretty e
  liftIO $ do
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> fname) content

saveLLVM :: FilePath -> PipelineM ()
saveLLVM fname' = do
  e <- use psExp
  n <- use psTransStep
  Just hptResult <- use psHPTResult
  o <- view poOutputDir
  let fname = o </> concat [fname',".",show n]
      code = CGLLVM.codeGen hptResult e
      llName = printf "%s.ll" fname
      sName = printf "%s.s" fname
  liftIO . void $ do
    Text.putStrLn $ ppllvm code
    putStrLn "* to LLVM *"
    _ <- CGLLVM.toLLVM llName code
    putStrLn "* LLVM X64 codegen *"
    callProcess "llc-5.0" [llName]
    readFile sName >>= putStrLn

debugTransformation :: (Exp -> Exp) -> PipelineM ()
debugTransformation t = do
  e <- use psExp
  liftIO . putStrLn . show $ pretty (t e)

check :: PipelineM ()
check = do
  e <- use psExp
  let nonUnique = nonUniqueNames e
  liftIO $ putStrLn $ unwords ["Non unique names:", show nonUnique]
  let nonDefined = nonDefinedNames e
  liftIO . putStrLn $ unwords ["Non defined names:", show nonDefined]

pipeline :: PipelineOpts -> Exp -> [Pipeline] -> IO ()
pipeline o e p = do
  print p
  flip evalStateT start .
    flip runReaderT o   .
    sequence_           .
    intersperse check $ Prelude.map pipelineStep p
  where
    start = PState e 0 Nothing Nothing
