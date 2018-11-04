{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, PatternSynonyms #-}
module Pipeline.Pipeline where

import Prelude
import Control.Monad
import Data.Maybe (maybe, fromJust, fromMaybe)
import Text.Printf
import Text.Pretty.Simple (pPrint)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))
import qualified Text.Show.Pretty as PP

import Pipeline.Eval
import Grin.Grin
import Grin.TypeEnv
import Grin.TypeCheck
import Pipeline.Optimizations
import qualified Grin.Statistics as Statistics
import Grin.Pretty()
import Transformations.CountVariableUse
import Transformations.GenerateEval
import qualified Transformations.Simplifying.Vectorisation2 as Vectorisation2
import Transformations.Simplifying.Vectorisation
import Transformations.BindNormalisation
import qualified Grin.Lint as Lint
import Grin.PrettyLint
import Transformations.Simplifying.SplitFetch
import Transformations.Simplifying.CaseSimplification
import Transformations.Optimising.Inlining (inlineEval, inlineApply, inlineBuiltins)
import Transformations.UnitPropagation
import Transformations.MangleNames
import Transformations.EffectMap
import qualified Transformations.Simplifying.RightHoistFetch2 as RHF
import Transformations.Simplifying.RegisterIntroduction
import qualified AbstractInterpretation.HPTResult as HPT
import AbstractInterpretation.PrettyHPT
import qualified AbstractInterpretation.PrettyIR as HPT
import qualified AbstractInterpretation.IR as HPT
import qualified AbstractInterpretation.CodeGenMain as HPT
import qualified AbstractInterpretation.Reduce as HPT
import qualified Reducer.LLVM.CodeGen as CGLLVM
import qualified Reducer.LLVM.JIT as JITLLVM
import System.Directory
import System.Process
import Data.Bifunctor

import qualified Data.Bimap as Bimap
import Data.Map as Map
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as Text

import Control.Monad.State as MonadState (get, put)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Lens.Micro.TH
import Lens.Micro.Mtl
import Data.Set
import System.FilePath
import System.Exit
import Control.DeepSeq
import Debug.Trace
import Lens.Micro
import Data.List

import Grin.Lint

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Control.Monad.Extra
import System.Random
import Data.Time.Clock

type RenameVariablesMap = Map String String

data Transformation
  -- Simplifying
  = RegisterIntroduction
  | Vectorisation
  | SplitFetch
  | CaseSimplification
  | RightHoistFetch
  | InlineEval
  | InlineApply
  | InlineBuiltins
  -- Misc
  | GenerateEval
  | BindNormalisation
  | ConstantFolding
  | UnitPropagation
  | MangleNames
  -- Optimizations
  | EvaluatedCaseElimination
  | TrivialCaseElimination
  | SparseCaseOptimisation
  | UpdateElimination
  | NonSharedElimination
  | CopyPropagation
  | ConstantPropagation
  | DeadProcedureElimination
  | DeadParameterElimination
  | DeadVariableElimination
  | CommonSubExpressionElimination
  | CaseCopyPropagation
  | CaseHoisting
  | GeneralizedUnboxing
  | ArityRaising
  | LateInlining
  deriving (Enum, Eq, Ord, Show)

noTypeEnv :: (Exp -> Exp) -> (TypeEnv, Exp) -> (TypeEnv, Exp)
noTypeEnv f (t, e) = (t, f e)

noEffectMap :: ((TypeEnv, Exp) -> (TypeEnv, Exp)) -> (TypeEnv, EffectMap, Exp) -> (TypeEnv, EffectMap, Exp)
noEffectMap f (te0, em0, e0) = let (te1, e1) = f (te0, e0) in (te1, em0, e1)

transformation :: Int -> Transformation -> (TypeEnv, EffectMap, Exp) -> (TypeEnv, EffectMap, Exp)
transformation n = \case
  Vectorisation                   -> noEffectMap Vectorisation2.vectorisation
  GenerateEval                    -> noEffectMap $ noTypeEnv generateEval
  CaseSimplification              -> noEffectMap $ noTypeEnv caseSimplification
  SplitFetch                      -> noEffectMap $ noTypeEnv splitFetch
  RegisterIntroduction            -> noEffectMap $ noTypeEnv $ registerIntroductionI n
  RightHoistFetch                 -> noEffectMap $ noTypeEnv RHF.rightHoistFetch
  -- misc
  MangleNames                     -> noEffectMap $ noTypeEnv mangleNames
  -- optimising
  BindNormalisation               -> noEffectMap $ noTypeEnv bindNormalisation
  ConstantFolding                 -> noEffectMap $ noTypeEnv constantFolding
  EvaluatedCaseElimination        -> noEffectMap $ noTypeEnv evaluatedCaseElimination
  TrivialCaseElimination          -> noEffectMap $ noTypeEnv trivialCaseElimination
  UpdateElimination               -> noEffectMap $ noTypeEnv updateElimination
  CopyPropagation                 -> noEffectMap $ noTypeEnv copyPropagation
  ConstantPropagation             -> noEffectMap $ noTypeEnv constantPropagation
  DeadProcedureElimination        -> noEffectMap $ noTypeEnv deadProcedureElimination
  DeadParameterElimination        -> noEffectMap $ noTypeEnv deadParameterElimination
  InlineEval                      -> noEffectMap inlineEval
  InlineApply                     -> noEffectMap inlineApply
  InlineBuiltins                  -> noEffectMap inlineBuiltins
  SparseCaseOptimisation          -> noEffectMap sparseCaseOptimisation
  DeadVariableElimination         -> deadVariableElimination
  CommonSubExpressionElimination  -> noEffectMap commonSubExpressionElimination
  CaseCopyPropagation             -> noEffectMap $ noTypeEnv caseCopyPropagation
  CaseHoisting                    -> noEffectMap caseHoisting
  GeneralizedUnboxing             -> noEffectMap generalizedUnboxing
  ArityRaising                    -> noEffectMap arityRaising
  LateInlining                    -> noEffectMap lateInlining
  UnitPropagation                 -> noEffectMap unitPropagation
  NonSharedElimination            -> noEffectMap nonSharedElimination

newtype Hidden a = H a

instance Show (Hidden a) where
  show _ = "(hidden)"

instance Eq (Hidden a) where
  _ == _ = True

data HPTStep
  = CompileHPT
  | PrintHPTCode
  | RunHPTPure
  | PrintHPTResult
  deriving (Eq, Show)

data EffectStep
  = CalcEffectMap
  | PrintEffectMap
  deriving (Eq, Show)

data PipelineStep
  = HPT HPTStep
  | Eff EffectStep
  | T Transformation
  | Pass [PipelineStep]
  | PrintGrinH (Hidden (Doc -> Doc))
  | PureEval
  | JITLLVM
  | PrintAST
  | SaveLLVM Bool FilePath
  | SaveGrin FilePath
  | DebugTransformationH (Hidden (Exp -> Exp))
  | Statistics
  | PrintTypeEnv
  | SaveTypeEnv
  | Lint
  | ConfluenceTest
  | DebugPipelineState
  deriving (Eq, Show)

pattern PrintGrin :: (Doc -> Doc) -> PipelineStep
pattern PrintGrin c <- PrintGrinH (H c)
  where PrintGrin c =  PrintGrinH (H c)

pattern DebugTransformation :: (Exp -> Exp) -> PipelineStep
pattern DebugTransformation t <- DebugTransformationH (H t)
  where DebugTransformation t =  DebugTransformationH (H t)

data PipelineOpts = PipelineOpts
  { _poOutputDir    :: FilePath
  , _poFailOnLint   :: Bool
  , _poLogging      :: Bool
  , _poSaveTypeEnv  :: Bool
  , _poStatistics   :: Bool
  }

defaultOpts :: PipelineOpts
defaultOpts = PipelineOpts
  { _poOutputDir    = ".grin-output"
  , _poFailOnLint   = True
  , _poLogging      = True
  , _poSaveTypeEnv  = False
  , _poStatistics   = False
  }

type PipelineM a = ReaderT PipelineOpts (StateT PState IO) a
data PState = PState
    { _psExp        :: Exp
    , _psTransStep  :: Int
    , _psSaveIdx    :: Int
    , _psHPTProgram :: Maybe HPT.HPTProgram
    , _psHPTResult  :: Maybe HPT.HPTResult
    , _psTypeEnv    :: Maybe TypeEnv
    , _psEffectMap  :: Maybe EffectMap
    , _psErrors     :: [String]
    } deriving (Show)

makeLenses ''PState
makeLenses ''PipelineOpts

data PipelineEff
  = None
  | ExpChanged
  deriving (Eq, Show)

_None :: Traversal' PipelineEff ()
_None f None = const None <$> f ()
_None _ rest = pure rest

_ExpChanged :: Traversal' PipelineEff ()
_ExpChanged f ExpChanged = const ExpChanged <$> f ()
_ExpChanged _ rest       = pure rest

pipelineLog :: String -> PipelineM ()
pipelineLog str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStrLn str

pipelineLog1 :: String -> PipelineM ()
pipelineLog1 str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStr str


pipelineStep :: PipelineStep -> PipelineM PipelineEff
pipelineStep p = do
  case p of
    Pass{}  -> pure () -- each pass step will be printed anyway
    _       -> pipelineLog1 $ printf "PipelineStep: %-50s" (show p)
  before <- use psExp
  start <- liftIO getCurrentTime
  case p of
    HPT hptStep -> case hptStep of
      CompileHPT      -> compileHPT
      PrintHPTCode    -> printHPTCode
      RunHPTPure      -> runHPTPure
      PrintHPTResult  -> printHPTResult
    Eff eff -> case eff of
      CalcEffectMap   -> calcEffectMap
      PrintEffectMap  -> printEffectMap
    T t             -> transformationM t
    Pass pass       -> mapM_ pipelineStep pass
    PrintGrin d     -> printGrinM d
    PureEval        -> pureEval
    JITLLVM         -> jitLLVM
    SaveLLVM relPath path -> saveLLVM relPath path
    SaveGrin path   -> saveGrin path
    PrintAST        -> printAST
    PrintTypeEnv    -> printTypeEnv
    SaveTypeEnv     -> saveTypeEnv
    DebugTransformation t -> debugTransformation t
    Statistics      -> statistics
    Lint            -> lintGrin Nothing
    ConfluenceTest  -> confluenceTest
    DebugPipelineState -> debugPipelineState
  after <- use psExp
  let eff = if before == after then None else ExpChanged
  end <- liftIO getCurrentTime
  case p of
    Pass{} -> pure ()
    T{} -> pipelineLog $ printf "had effect: %s (%s)"
              (show eff) (show $ diffUTCTime end start)
    _   -> pipelineLog $ printf "(%s)" (show $ diffUTCTime end start)
  -- TODO: Test this only for development mode.
  return eff


calcEffectMap :: PipelineM ()
calcEffectMap = do
  grin <- use psExp
  env0 <- fromMaybe (traceShow "emptyTypEnv is used" emptyTypeEnv) <$> use psTypeEnv
  psEffectMap .= Just (effectMap (env0, grin))

printEffectMap :: PipelineM ()
printEffectMap = do
  grin <- use psExp
  env0 <- fromMaybe (traceShow "emptyTypEnv is used" emptyTypeEnv) <$> use psTypeEnv
  pipelineLog $ show $ pretty env0

compileHPT :: PipelineM ()
compileHPT = do
  grin <- use psExp
  case HPT.codeGen grin of
    Right hptProgram -> do
      psHPTProgram .= Just hptProgram
    Left e -> do
      psErrors %= (e:)
      psHPTProgram .= Nothing
  {-
  let nonlinearSet  = nonlinearVariables grin
      countMap      = countVariableUse grin
  --pPrint countMap
  --pPrint nonlinearSet
  liftIO $ putStrLn "non-linear variables:"
  liftIO $ print . pretty $ nonlinearSet
  -}

printHPTCode :: PipelineM ()
printHPTCode = do
  hptProgram <- use psHPTProgram
  let printHPT a = do
        pipelineLog $ show $ HPT.prettyInstructions (Just a) . HPT.hptInstructions $ a
        pipelineLog $ printf "memory size    %d" $ HPT.hptMemoryCounter a
        pipelineLog $ printf "register count %d" $ HPT.hptRegisterCounter a
        pipelineLog $ printf "variable count %d" $ Map.size $ HPT.hptRegisterMap a
  maybe (pure ()) printHPT hptProgram

printHPTResult :: PipelineM ()
printHPTResult = use psHPTResult >>= \case
  Nothing -> pure ()
  Just result -> pipelineLog $ show $ pretty $ result

runHPTPure :: PipelineM ()
runHPTPure = use psHPTProgram >>= \case
  Nothing -> psHPTResult .= Nothing
  Just hptProgram -> do
    let hptResult = HPT.evalHPT hptProgram
        result = HPT.toHPTResult hptProgram hptResult
    psHPTResult .= Just result
    case typeEnvFromHPTResult result of
      Right te  -> psTypeEnv .= Just te
      Left err  -> do
        psErrors %= (err :)
        psTypeEnv .= Nothing

printTypeEnv :: PipelineM ()
printTypeEnv = do
  Just typeEnv <- use psTypeEnv
  pipelineLog $ show $ pretty $ typeEnv

saveTypeEnv :: PipelineM ()
saveTypeEnv = do
  mTypeEnv <- use psTypeEnv
  forM_ mTypeEnv $ \typeEnv -> do
    n <- use psSaveIdx
    outputDir <- view poOutputDir
    let fname = printf "%03d.Type-Env" n
    let content = show $ plain $ pretty typeEnv
    liftIO $ do
      writeFile (outputDir </> fname) content

statistics :: PipelineM ()
statistics = do
  exp <- use psExp
  n <- use psSaveIdx
  outputDir <- view poOutputDir
  let fname = printf "%03d.Statistics" n
  let content = show $ plain $ pretty $ Statistics.statistics exp
  liftIO $ do
    writeFile (outputDir </> fname) content

transformationM :: Transformation -> PipelineM ()
transformationM t = do
  --preconditionCheck t
  env0 <- fromMaybe (traceShow "emptyTypEnv is used" emptyTypeEnv) <$> use psTypeEnv
  effs0 <- fromMaybe (traceShow "emptyEffectMap is used" mempty) <$> use psEffectMap
  n    <- use psTransStep
  exp0 <- use psExp
  let (env1, effs1, exp1) = transformation n t (env0, effs0, exp0)
  psTypeEnv .= Just env1
  psExp     .= exp1
  psTransStep %= (+1)
  --postconditionCheck t

pureEval :: PipelineM ()
pureEval = do
  e <- use psExp
  val <- liftIO $ evalProgram PureReducer e
  pipelineLog $ show $ pretty val

printGrinM :: (Doc -> Doc) -> PipelineM ()
printGrinM color = do
  e <- use psExp
  pipelineLog $ show $ color $ pretty e

jitLLVM :: PipelineM ()
jitLLVM = do
  e <- use psExp
  Just typeEnv <- use psTypeEnv
  val <- liftIO $ JITLLVM.eagerJit (CGLLVM.codeGen typeEnv e) "grinMain"
  pipelineLog $ show $ pretty val

printAST :: PipelineM ()
printAST = do
  e <- use psExp
  pPrint e

saveGrin :: FilePath -> PipelineM ()
saveGrin fn = do
  psSaveIdx %= succ
  n <- use psSaveIdx
  e <- use psExp
  outputDir <- view poOutputDir
  let fname = printf "%03d.%s" n fn
  let content = show $ plain $ pretty e
  liftIO $ do
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> fname) content

saveLLVM :: Bool -> FilePath -> PipelineM ()
saveLLVM relPath fname' = do
  e <- use psExp
  psSaveIdx %= succ
  n <- use psSaveIdx
  Just typeEnv <- use psTypeEnv
  o <- view poOutputDir
  let fname = if relPath then o </> printf "%03d.%s" n fname' else fname'
      code = CGLLVM.codeGen typeEnv e
      llName = printf "%s.ll" fname
      sName = printf "%s.s" fname
  liftIO . void $ do
    Text.putStrLn $ ppllvm code
    putStrLn "* to LLVM *"
    _ <- CGLLVM.toLLVM llName code
    putStrLn "* LLVM X64 codegen *"
    callCommand $ printf "opt-5.0 -O3 %s | llc-5.0 -o %s" llName sName
    readFile sName >>= putStrLn

debugTransformation :: (Exp -> Exp) -> PipelineM ()
debugTransformation t = do
  e <- use psExp
  liftIO . print $ pretty (t e)

lintGrin :: Maybe String -> PipelineM ()
lintGrin mPhaseName = do
  pipelineStep $ HPT CompileHPT
  pipelineStep $ HPT RunHPTPure
  exp <- use psExp
  mTypeEnv <- use psTypeEnv
  let lintExp@(_, errorMap) = Lint.lint mTypeEnv exp
  when (Map.size errorMap > 0) $ do
    psErrors %= ((concat $ Map.elems errorMap) ++)

  -- print errors
  errors <- use psErrors
  unless (Prelude.null errors) $ void $ do
    failOnLintError <- view poFailOnLint
    when failOnLintError $ void $ do
      pipelineLog $ show $ prettyLintExp lintExp
      pipelineStep $ HPT PrintHPTResult
    case mPhaseName of
      Just phaseName  -> pipelineLog $ printf "error after %s:\n%s" phaseName (unlines errors)
      Nothing         -> pipelineLog $ printf "error:\n%s" (unlines errors)

    failOnLintError <- view poFailOnLint
    when failOnLintError $ do
      liftIO $ die "illegal code"

-- confluence testing

-- Generate random pipeline based on the transformationWhitelist, the pipeline reaches a fixpoint
-- and returns the list of transformation that helped to reach the fixpoint.
randomPipeline :: PipelineM [Transformation]
randomPipeline = do
  mapM_ pipelineStep
    [ HPT CompileHPT
    , HPT RunHPTPure
    , Eff CalcEffectMap
    ]
  go transformationWhitelist []
  where
    go :: [Transformation] -> [Transformation] -> PipelineM [Transformation]
    go [] result = pure $ reverse result
    go available res = do
      t <- fmap ((available !!) . abs . (`mod` (length available))) $ liftIO $ randomIO
      eff <- pipelineStep (T t)
      case eff of
        None -> go (available Data.List.\\ [t]) res
        ExpChanged -> do
          lintGrin . Just $ show t
          mapM_ pipelineStep
            [ HPT CompileHPT
            , HPT RunHPTPure
            , Eff CalcEffectMap
            ]
          go transformationWhitelist (t:res)

    transformationWhitelist :: [Transformation]
    transformationWhitelist =
        -- Misc
        [ UnitPropagation
        -- Optimizations
        , EvaluatedCaseElimination
        , TrivialCaseElimination
        , SparseCaseOptimisation
        , UpdateElimination
        , CopyPropagation
        , ConstantPropagation
        , DeadProcedureElimination
        , DeadParameterElimination
        , DeadVariableElimination
        , CommonSubExpressionElimination
        , CaseCopyPropagation
        , CaseHoisting
        , GeneralizedUnboxing
        , ArityRaising
        , LateInlining
        ]

confluenceTest :: PipelineM ()
confluenceTest = do
  pipelineLog "Confluence test"
  pipelineLog "Random pipeline #1"
  state <- MonadState.get
  pipeline1 <- randomPipeline
  pipelineLog "Random pipeline #2"
  exp1 <- use psExp
  MonadState.put state
  pipeline2 <- randomPipeline
  exp2 <- use psExp
  if (mangleNames exp1 /= mangleNames exp2)
    then do
      let [lines1, lines2] = lines . show . plain . pretty <$> [exp1, exp2]
      pipelineLog "\nDiff between transformed codes:"
      pipelineLog $ ppDiff $ getGroupedDiff lines1 lines2
    else
      pipelineLog "The calculated fixpoint is the same for the pipelines:"
  pipelineLog "First tranformation permutation:"
  pipelineLog $ show pipeline1
  pipelineLog "\nSecond transformation permutation:"
  pipelineLog $ show pipeline2

debugPipelineState :: PipelineM ()
debugPipelineState = do
  ps <- MonadState.get
  liftIO $ print ps

runPipeline :: PipelineOpts -> Exp -> PipelineM a -> IO (a, Exp)
runPipeline o e m = fmap (second _psExp) $ flip runStateT start $ runReaderT m o where
  start = PState
    { _psExp        = e
    , _psTransStep  = 0
    , _psSaveIdx    = 0
    , _psHPTProgram = Nothing
    , _psHPTResult  = Nothing
    , _psTypeEnv    = Nothing
    , _psEffectMap  = Nothing
    , _psErrors     = []
    }

-- | Runs the pipeline and returns the last version of the given
-- expression.
pipeline :: PipelineOpts -> Exp -> [PipelineStep] -> IO ([(PipelineStep, PipelineEff)], Exp)
pipeline o e ps = do
  print ps
  runPipeline o e $ mapM (\p -> (,) p <$> pipelineStep p) ps

-- | Run the pipeline with the given set of transformations, till
-- it reaches a fixpoint where none of the pipeline transformations
-- changes the expression itself, the order of the transformations
-- are defined in the pipeline list. After all round the TypeEnv
-- is restored
optimizeWithPM :: PipelineOpts -> Exp -> [PipelineStep] -> PipelineM ()
optimizeWithPM o e ps = loop where
  loop = do
    -- Run every step and on changes run HPT
    effs <- forM ps $ \p -> do
      eff <- pipelineStep p
      when (eff == ExpChanged) $ void $ do
        pipelineStep $ SaveGrin (fmap (\case ' ' -> '-' ; c -> c) $ show p)
        lintGrin . Just $ show p
        pipelineStep $ Eff CalcEffectMap
        when (o ^. poStatistics) $ void $ pipelineStep Statistics
        when (o ^. poSaveTypeEnv) $ void $ pipelineStep SaveTypeEnv
      pure eff
    -- Run loop again on change
    when (any (match _ExpChanged) effs)
      loop

optimize :: PipelineOpts -> Exp -> [PipelineStep] -> [PipelineStep] -> IO Exp
optimize o e pre post = optimizeWith o e pre optimizations post where
  optimizations =
    [ BindNormalisation
    , EvaluatedCaseElimination
    , TrivialCaseElimination
    , SparseCaseOptimisation
    , UpdateElimination
    , NonSharedElimination
    , CopyPropagation
    , ConstantPropagation
    , DeadProcedureElimination
    , DeadVariableElimination
    , DeadParameterElimination
    , CommonSubExpressionElimination
    , CaseCopyPropagation
    , CaseHoisting
    , GeneralizedUnboxing
    , ArityRaising
    , InlineEval
    , InlineApply
    , LateInlining
    ]

optimizeWith :: PipelineOpts -> Exp -> [PipelineStep] -> [Transformation] -> [PipelineStep] -> IO Exp
optimizeWith o e pre optimizations post = fmap snd $ runPipeline o e $ do
  lintGrin $ Just "init"

  mapM_ pipelineStep pre

  mapM_ pipelineStep
    [ HPT CompileHPT
    , HPT RunHPTPure
    , T UnitPropagation
    , Eff CalcEffectMap
    ]
  optimizeWithPM o e $ fmap T optimizations
  mapM_ pipelineStep post
