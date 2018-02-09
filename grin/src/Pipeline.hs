{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, PatternSynonyms #-}
module Pipeline where

import Control.Monad
import Data.List (intersperse)
import Data.Maybe (maybe)
import Text.Printf
import Text.Pretty.Simple (pPrint)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))
import qualified Text.Show.Pretty as PP

import Check hiding (check)
import Eval
import Grin
import Optimizations
import Pretty()
import Transformations.AssignStoreIDs
import Transformations.GenerateEval
import Transformations.Simplifying.Vectorisation
import Transformations.BindNormalisation
import Transformations.Simplifying.SplitFetch
import Transformations.Simplifying.CaseSimplification
import Transformations.Simplifying.RightHoistFetch
import Transformations.Simplifying.RegisterIntroduction
import Transformations.Playground
import AbstractInterpretation.AbstractRunGrin
import AbstractInterpretation.HPTResult
import AbstractInterpretation.PrettyHPT
import qualified AbstractInterpretation.Pretty as HPT
import qualified AbstractInterpretation.CodeGen as HPT
import qualified AbstractInterpretation.Reduce as HPT
import qualified Reducer.LLVM.CodeGen as CGLLVM
import qualified Reducer.LLVM.JIT as JITLLVM
import System.Directory
import System.Process

import qualified Data.Bimap as Bimap
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
  -- Optimizations
  | ConstantFolding
  deriving (Enum, Eq, Ord, Show)

transformation :: HPTResult -> Int -> Transformation -> Exp -> Exp
transformation hptResult n = \case
  CaseSimplification      -> caseSimplification
  SplitFetch              -> splitFetch
  Vectorisation           -> vectorisation hptResult
  RegisterIntroduction    -> registerIntroductionI n
  BindNormalisation       -> bindNormalisation
  RightHoistFetch         -> rightHoistFetch
  GenerateEval            -> generateEval
  ConstantFolding         -> constantFolding

precondition :: Transformation -> [Check]
precondition = \case
  CaseSimplification -> []
  SplitFetch -> []
  Vectorisation -> []
  RegisterIntroduction -> []
  BindNormalisation -> []
  RightHoistFetch -> []
  GenerateEval -> []
  ConstantFolding -> []


postcondition :: Transformation -> [Check]
postcondition = \case
  CaseSimplification -> []
  SplitFetch -> []
  Vectorisation -> [OnlyExplicitNodes]
  RegisterIntroduction -> []
  BindNormalisation -> []
  RightHoistFetch -> []
  GenerateEval -> []
  ConstantFolding -> []


newtype Hidden a = H a

instance Show (Hidden a) where
  show _ = "(hidden)"

instance Eq (Hidden a) where
  _ == _ = True

data HPTStep
  = CompileHPT
  | PrintHPT
  | RunHPTPure
  | PrintHPTResult
  deriving Show

data Pipeline
  = HPT HPTStep
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

defaultOpts :: PipelineOpts
defaultOpts = PipelineOpts
  { _poOutputDir = "./"
  }

type TagInfo = Set Tag
type PipelineM a = ReaderT PipelineOpts (StateT PState IO) a
data PState = PState
    { _psExp        :: Exp
    , _psTransStep  :: Int
    , _psHPTProgram :: Maybe HPT.HPTProgram
    , _psHPTResult  :: Maybe HPTResult
    , _psTagInfo    :: Maybe TagInfo
    }

makeLenses ''PState
makeLenses ''PipelineOpts

pipelineStep :: Pipeline -> PipelineM ()
pipelineStep = \case
  HPT hptStep -> case hptStep of
    CompileHPT      -> compileHPT
    PrintHPT        -> printHPT
    RunHPTPure      -> runHPTPure
    PrintHPTResult  -> printHPTResult
  T t             -> transformationM t
  TagInfo         -> tagInfo
  PrintGrin d     -> printGrinM d
  PureEval        -> pureEval
  JITLLVM         -> jitLLVM
  SaveLLVM path   -> saveLLVM path
  SaveGrin path   -> saveGrin path
  PrintAST        -> printAST
  DebugTransformation t -> debugTransformation t

compileHPT :: PipelineM ()
compileHPT = do
  grin <- use psExp
  let hptProgram = HPT.codeGen grin
  psHPTProgram .= Just hptProgram

printHPT :: PipelineM ()
printHPT = do
  hptProgram <- use psHPTProgram
  let printHPT a = do
        putStrLn . show . pretty . HPT.envInstructions $ a
        putStrLn $ printf "memory size    %d" $ HPT.envMemoryCounter a
        putStrLn $ printf "register count %d" $ HPT.envRegisterCounter a
        putStrLn $ printf "variable count %d" $ Bimap.size $ HPT.envRegisterMap a
  maybe (pure ()) (liftIO . printHPT) hptProgram

runHPTPure :: PipelineM ()
runHPTPure = do
  hptProgram <- use psHPTProgram
  let printHPT a = do
        let hptResult = HPT.evalHPT a
            Just hptProg = hptProgram
        --PP.pPrint hptResult
        putStrLn . show . pretty $ HPT.toHPTResult hptProg hptResult
  maybe (pure ()) (liftIO . printHPT) hptProgram

  grin <- use psExp
  let (_, result) = abstractRun (assignStoreIDs grin) "grinMain"
  psHPTResult .= Just result

printHPTResult :: PipelineM ()
printHPTResult = do
  hptResult <- use psHPTResult
  --maybe (pure ()) (liftIO . putStrLn . show . pretty) hptResult
  pure ()

preconditionCheck :: Transformation -> PipelineM ()
preconditionCheck t = do
  hpt <- use psHPTResult
  exp <- use psExp
  forM_ (checks hpt (precondition t) exp) $ \case
    (c, r) -> liftIO . putStrLn $ unwords ["The", show c, "precondition of", show t, ": ", show r]

postconditionCheck :: Transformation -> PipelineM ()
postconditionCheck t = do
  hpt <- use psHPTResult
  exp <- use psExp
  forM_ (checks hpt (postcondition t) exp) $ \case
    (c, r) -> liftIO . putStrLn $ unwords ["The", show c, "postcondition of", show t, ": ", show r]

transformationM :: Transformation -> PipelineM ()
transformationM t = do
  preconditionCheck t
  Just result <- use psHPTResult
  n           <- use psTransStep
  psExp       %= transformation result n t
  psTransStep %= (+1)
  postconditionCheck t

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

-- | Runs the pipeline and returns the last version of the given
-- expression.
pipeline :: PipelineOpts -> Exp -> [Pipeline] -> IO Exp
pipeline o e p = do
  print p
  fmap _psExp .
    flip execStateT start .
    flip runReaderT o   .
    sequence_           .
    intersperse check $ Prelude.map pipelineStep p
  where
    start = PState
      { _psExp        = e
      , _psTransStep  = 0
      , _psHPTProgram = Nothing
      , _psHPTResult  = Nothing
      , _psTagInfo    = Nothing
      }
