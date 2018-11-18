{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Pipeline.Pipeline
 ( module Pipeline.Pipeline 
 , module Pipeline.Definitions
 , module Pipeline.Utils
 ) where

import Prelude
import Control.Monad
import Data.Maybe (maybe, fromJust, fromMaybe)
import Text.Printf
import Text.Pretty.Simple (pPrint)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))
import qualified Text.Show.Pretty as PP

import Pipeline.Eval
import Pipeline.Definitions
import Pipeline.Utils
import Grin.Grin
import Grin.TypeEnv
import Grin.TypeCheck
import Grin.EffectMap
import Pipeline.Optimizations
import qualified Grin.Statistics as Statistics
import Grin.Parse
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
import Transformations.Simplifying.ProducerNameIntroduction
import qualified AbstractInterpretation.HPTResult as HPT
import qualified AbstractInterpretation.CByResult as CBy
import qualified AbstractInterpretation.LVAResult as LVA
import AbstractInterpretation.PrettyCBy
import AbstractInterpretation.PrettyHPT
import AbstractInterpretation.PrettyLVA
import qualified AbstractInterpretation.PrettyIR as HPT
<<<<<<< HEAD
import qualified AbstractInterpretation.IR as IR
import qualified AbstractInterpretation.HeapPointsTo as HPT
import qualified AbstractInterpretation.CreatedBy    as CBy
import qualified AbstractInterpretation.LiveVariable as LVA
import qualified AbstractInterpretation.Reduce as R
=======
import qualified AbstractInterpretation.IR as HPT
import qualified AbstractInterpretation.CodeGenMain as HPT
import qualified AbstractInterpretation.Reduce as HPT
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
import qualified Reducer.LLVM.CodeGen as CGLLVM
import qualified Reducer.LLVM.JIT as JITLLVM
import System.Directory
import System.Process
import Data.Bifunctor

import qualified Data.Bimap as Bimap
import Data.Map as Map
import LLVM.Pretty (ppllvm)
import qualified Data.Text.Lazy.IO as Text

import Control.Monad.State.Class as MonadState (get, put, gets)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict hiding (gets)
import Control.Monad.IO.Class
import Lens.Micro.TH
import Lens.Micro.Mtl
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
import Data.Fixed

<<<<<<< HEAD
=======
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

>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
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
  ProducerNameIntroduction        -> noEffectMap $ noTypeEnv producerNameIntroduction
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
  SimpleDeadFunctionElimination   -> noEffectMap $ noTypeEnv simpleDeadFunctionElimination
  SimpleDeadParameterElimination  -> noEffectMap $ noTypeEnv simpleDeadParameterElimination
  InlineEval                      -> noEffectMap inlineEval
  InlineApply                     -> noEffectMap inlineApply
  InlineBuiltins                  -> noEffectMap inlineBuiltins
  SimpleDeadVariableElimination   -> simpleDeadVariableElimination
  CommonSubExpressionElimination  -> noEffectMap commonSubExpressionElimination
  CaseCopyPropagation             -> noEffectMap $ noTypeEnv caseCopyPropagation
  CaseHoisting                    -> noEffectMap caseHoisting
  GeneralizedUnboxing             -> noEffectMap generalizedUnboxing
  ArityRaising                    -> noEffectMap arityRaising
  LateInlining                    -> noEffectMap lateInlining
  UnitPropagation                 -> noEffectMap unitPropagation
  NonSharedElimination            -> noEffectMap nonSharedElimination

<<<<<<< HEAD
=======
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

pipelineLogNoLn :: String -> PipelineM ()
pipelineLogNoLn str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStr str


>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
pipelineStep :: PipelineStep -> PipelineM PipelineEff
pipelineStep p = do
  let specialPrint = [HPT PrintHPTCode, HPT PrintHPTResult, PrintGrin id, PrintAST, PrintTypeEnv]
  case p of
    Pass{}  -> pure () -- each pass step will be printed anyway
    _ | p `elem` specialPrint -> pipelineLog $ printf "PipelineStep: %-50s" (show p)
    _ -> pipelineLogNoLn $ printf "PipelineStep: %-50s" (show p)
  before <- use psExp
  start <- liftIO getCurrentTime
  case p of
    Optimize -> do 
      let opts = defaultOpts { _poFailOnLint = True }
          prePipeline = defaultOnChange
      grin <- use psExp
      mapM_ pipelineStep prePipeline
      optimizeWithPM opts grin (fmap T defaultOptimizations) defaultOnChange defaultCleanUp
    HPT step -> case step of
      CompileToAbstractProgram -> compileHPT
      PrintAbstractProgram     -> printHPTCode
      RunAbstractProgramPure   -> runHPTPure
      PrintAbstractResult      -> printHPTResult
    CBy step -> case step of
      CompileToAbstractProgram -> compileCBy
      PrintAbstractProgram     -> printCByCode
      RunAbstractProgramPure   -> runCByPure
      PrintAbstractResult      -> printCByResult
    LVA step -> case step of
      CompileToAbstractProgram -> compileLVA
      PrintAbstractProgram     -> printLVACode
      RunAbstractProgramPure   -> runLVAPure
      PrintAbstractResult      -> printLVAResult
    RunCByWithLVA -> runCByWithLVAPure
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
    ParseTypeAnnots -> parseTypeAnnots
    PrintTypeAnnots -> printTypeAnnots
    PrintTypeEnv    -> printTypeEnv
    SaveTypeEnv     -> saveTypeEnv
    DebugTransformation t -> debugTransformation t
    Statistics      -> statistics
    Lint            -> lintGrin Nothing
    ConfluenceTest  -> confluenceTest
<<<<<<< HEAD
    PrintErrors     -> do 
      errors <- use psErrors
      pipelineLog $ unlines $ "errors:" : errors
=======
    DebugPipelineState -> debugPipelineState
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
  after <- use psExp
  let eff = if before == after then None else ExpChanged
      showMS :: Rational -> String
      showMS t = printf "%.6f ms" (realToFrac $ 1E3 * t :: Double)

  end <- liftIO getCurrentTime
  case p of
    Pass{} -> pure ()
    T{} -> pipelineLog $ printf "had effect: %s (%s)"
              (show eff) (showMS $ toRational $ diffUTCTime end start)
    _   -> pipelineLog $ printf "(%s)" (showMS $ toRational $ diffUTCTime end start)
  -- TODO: Test this only for development mode.
  return eff


calcEffectMap :: PipelineM ()
calcEffectMap = do
  grin <- use psExp
  env0 <- fromMaybe (traceShow "Empty type environment is used to calculate effect map" emptyTypeEnv) <$> use psTypeEnv
  psEffectMap .= Just (effectMap (env0, grin))

printEffectMap :: PipelineM ()
printEffectMap = do
  effs <- fromMaybe (traceShow "No effect map is available" mempty) <$> use psEffectMap
  pipelineLog $ show $ pretty effs

compileHPT :: PipelineM ()
compileHPT = do
  grin <- use psExp
  case HPT.codeGen grin of
    Right hptProgram ->
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

printAbsProg a = do
  pipelineLog $ show $ HPT.prettyInstructions (Just a) . IR.absInstructions $ a
  pipelineLog $ printf "memory size    %d" $ IR.absMemoryCounter a
  pipelineLog $ printf "register count %d" $ IR.absRegisterCounter a
  pipelineLog $ printf "variable count %d" $ Map.size $ IR.absRegisterMap a

printHPTCode :: PipelineM ()
printHPTCode = do
  hptProgram <- use psHPTProgram
  maybe (pure ()) (printAbsProg . IR.getDataFlowInfo) hptProgram

printHPTResult :: PipelineM ()
printHPTResult = use psHPTResult >>= \case
  Nothing -> pure ()
  Just result -> pipelineLog $ show $ pretty result

runHPTPure :: PipelineM ()
runHPTPure = use psHPTProgram >>= \case
  Nothing -> psHPTResult .= Nothing
  Just hptProgram -> do
<<<<<<< HEAD
    let hptResult = R.evalDataFlowInfo hptProgram
=======
    let (hptResult, hptInfo) = HPT.evalHPT hptProgram
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
        result = HPT.toHPTResult hptProgram hptResult
    pipelineLogNoLn $ unwords ["iterations:", show (HPT.hptIterations hptInfo), ""]
    psHPTResult .= Just result
    case typeEnvFromHPTResult result of
      Right te  -> psTypeEnv .= Just te
      Left err  -> do
        psErrors %= (err :)
        psTypeEnv .= Nothing

compileCBy :: PipelineM ()
compileCBy = do
  grin <- use psExp
  case CBy.codeGen grin of
    Right cbyProgram ->
      psCByProgram .= Just cbyProgram
    Left e -> do
      psErrors %= (e:)
      psCByProgram .= Nothing

printCByCode :: PipelineM ()
printCByCode = do
  cbyProgM <- use psCByProgram
  maybe (pure ()) (printAbsProg . IR.getDataFlowInfo) cbyProgM

printCByResult :: PipelineM ()
printCByResult = use psCByResult >>= \case
  Nothing -> pure ()
  Just result -> pipelineLog $ show $ pretty result

runCByPureWith :: (CBy.CByProgram -> R.Computer -> CBy.CByResult) -> PipelineM ()
runCByPureWith toCByResult = use psCByProgram >>= \case
  Nothing -> psCByResult .= Nothing
  Just cbyProgram -> do
    let cbyResult = R.evalDataFlowInfo cbyProgram
        result = toCByResult cbyProgram cbyResult
    psCByResult .= Just result
    case typeEnvFromHPTResult (CBy._hptResult result) of
      Right te  -> psTypeEnv .= Just te
      Left err  -> do
        psErrors %= (err :)
        psTypeEnv .= Nothing

runCByPure :: PipelineM ()
runCByPure = runCByPureWith CBy.toCByResult

runCByWithLVAPure :: PipelineM ()
runCByWithLVAPure = do
  runLVAPure
  use psLVAResult >>= \case
    Nothing -> do 
      psCByResult .= Nothing
      psErrors %= ("LVA result is not availabe for cby-with-lva pass" :)
    Just lvaResult -> runCByPureWith (CBy.toCByResultWithLiveness lvaResult)


compileLVA :: PipelineM ()
compileLVA = do
  grin <- use psExp
  case LVA.codeGen grin of
    Right lvaProgram ->
      psLVAProgram .= Just lvaProgram
    Left e -> do
      psErrors %= (e:)
      psLVAProgram .= Nothing

printLVACode :: PipelineM ()
printLVACode = do
  lvaProgM <- use psLVAProgram
  maybe (pure ()) (printAbsProg . IR.getDataFlowInfo) lvaProgM

printLVAResult :: PipelineM ()
printLVAResult = use psLVAResult >>= \case
  Nothing -> pure ()
  Just result -> pipelineLog $ show $ pretty result

runLVAPure :: PipelineM ()
runLVAPure = use psLVAProgram >>= \case
  Nothing -> psLVAResult .= Nothing
  Just lvaProgram -> do
    let lvaResult = R.evalDataFlowInfo lvaProgram
        result = LVA.toLVAResult lvaProgram lvaResult
    psLVAResult .= Just result




parseTypeAnnots :: PipelineM () 
parseTypeAnnots = do 
  Just src <- use psSrc
  psTypeAnnots .= Just (parseMarkedTypeEnv src)

printTypeAnnots :: PipelineM () 
printTypeAnnots = do 
  Just typeEnv <- use psTypeAnnots
  pipelineLog . show . pretty $ typeEnv

printTypeEnv :: PipelineM ()
printTypeEnv = do
  Just typeEnv <- use psTypeEnv
  pipelineLog . show . pretty $ typeEnv


saveTransformationInfo :: (Pretty a) => String -> a -> PipelineM ()
saveTransformationInfo name content = do
  n <- use psSaveIdx
  outputDir <- view poOutputDir
  let fname = printf "%03d.%s" n name
  liftIO $ do
    writeFile (outputDir </> fname) $ show $ plain $ pretty content

saveTypeEnv :: PipelineM ()
saveTypeEnv = do
  mTypeEnv <- use psTypeEnv
  forM_ mTypeEnv $ saveTransformationInfo "Type-Env"

statistics :: PipelineM ()
statistics = do
  exp <- use psExp
  saveTransformationInfo "Statistics" $ Statistics.statistics exp

transformationM :: Transformation -> PipelineM ()
transformationM DeadCodeElimination = do 
  withTyEnvCByLVA $ \typeEnv cbyResult lvaResult -> do

    e <- use psExp
    case deadFunctionElimination lvaResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)
    
    e  <- use psExp
    case deadDataElimination lvaResult cbyResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

    e <- use psExp
    case deadVariableElimination lvaResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

    e <- use psExp
    case deadParameterElimination lvaResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

transformationM DeadFunctionElimination = do
  e  <- use psExp
  withTyEnvLVA $ \typeEnv lvaResult -> do
    case deadFunctionElimination lvaResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

transformationM DeadVariableElimination = do
  e  <- use psExp
  withTyEnvLVA $ \typeEnv lvaResult -> do
    case deadVariableElimination lvaResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

transformationM DeadParameterElimination = do
  e  <- use psExp
  withTyEnvLVA $ \typeEnv lvaResult -> do
    case deadParameterElimination lvaResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

transformationM DeadDataElimination = do
  e  <- use psExp
  withTyEnvCByLVA $ \typeEnv cbyResult lvaResult -> do
    case deadDataElimination lvaResult cbyResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

transformationM SparseCaseOptimisation = do 
  e  <- use psExp
  withTypeEnv $ \typeEnv ->
    case sparseCaseOptimisation typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)
  
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
  e <- use psExp
  saveTransformationInfo fn e

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
    callCommand $ printf "opt-7 -O3 %s | llc-7 -o %s" llName sName
    readFile sName >>= putStrLn

debugTransformation :: (Exp -> Exp) -> PipelineM ()
debugTransformation t = do
  e <- use psExp
  liftIO . print $ pretty (t e)

lintGrin :: Maybe String -> PipelineM ()
lintGrin mPhaseName = do
  pipelineStep $ HPT CompileToAbstractProgram
  pipelineStep $ HPT RunAbstractProgramPure
  exp <- use psExp
  mTypeEnv <- use psTypeEnv
  let lintExp@(_, errorMap) = Lint.lint mTypeEnv exp
  psErrors .= (fmap message $ concat $ Map.elems errorMap)

  -- print errors
  errors <- use psErrors
  unless (Prelude.null errors) $ void $ do
<<<<<<< HEAD
    failOnLintError <- view poFailOnLint
    when failOnLintError $ void $ do
      pipelineLog $ show $ prettyLintExp lintExp
      pipelineStep $ HPT PrintAbstractResult
=======
    saveTransformationInfo "Lint" $ prettyLintExp lintExp
    mHptResult <- use psHPTResult
    saveTransformationInfo "HPT-Result" mHptResult
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
    case mPhaseName of
      Just phaseName  -> pipelineLog $ printf "error after %s:\n%s" phaseName (unlines errors)
      Nothing         -> pipelineLog $ printf "error:\n%s" (unlines errors)
    failOnLintError <- view poFailOnLint
    when failOnLintError $ do
      -- FIXME: reenable after: undefined support ; transformation to inject default alts for pattern match errors
      -- liftIO $ die "illegal code"
      pure ()

-- confluence testing

-- Generate random pipeline based on the transformationWhitelist, the pipeline reaches a fixpoint
-- and returns the list of transformation that helped to reach the fixpoint.
<<<<<<< HEAD
randomPipeline :: PipelineM [Transformation]
randomPipeline = do
  runBasicAnalyses
=======
randomPipeline :: StdGen -> PipelineM [Transformation]
randomPipeline seed = do
  liftIO $ setStdGen seed
  mapM_ pipelineStep
    [ HPT CompileHPT
    , HPT RunHPTPure
    , Eff CalcEffectMap
    ]
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
  go transformationWhitelist []
  where
    go :: [Transformation] -> [Transformation] -> PipelineM [Transformation]
    go [] result = do
      -- The final result must be normalised as, non-normalised and normalised
      -- grin program is semantically the same.
      pipelineStep $ T BindNormalisation
      pure $ reverse result
    go available res = do
      t <- fmap ((available !!) . abs . (`mod` (length available))) $ liftIO $ randomIO
      when (needsCByLVA t) $ do 
        runNameIntro
        runCByLVA
      eff <- pipelineStep (T t)
      when (needsCleanup t) $ do 
        runCleanup
      case eff of
        None -> go (available Data.List.\\ [t]) res
        ExpChanged -> do
          lintGrin . Just $ show t
          runBasicAnalyses
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
        , ConstantPropagation
        , DeadCodeElimination
        , SimpleDeadFunctionElimination
        , SimpleDeadParameterElimination
        , CommonSubExpressionElimination
        , CaseCopyPropagation
        , CaseHoisting
        , GeneralizedUnboxing
        , ArityRaising
        , LateInlining
        ]

    runBasicAnalyses :: PipelineM ()
    runBasicAnalyses = mapM_ pipelineStep
      [ HPT CompileToAbstractProgram
      , HPT RunAbstractProgramPure
      , Eff CalcEffectMap
      ]

    runCByLVA :: PipelineM ()
    runCByLVA = mapM_ pipelineStep
      [ CBy CompileToAbstractProgram
      , CBy RunAbstractProgramPure
      , LVA CompileToAbstractProgram
      , LVA RunAbstractProgramPure
      , Eff CalcEffectMap
      ]

    runNameIntro :: PipelineM () 
    runNameIntro = void . pipelineStep $ Pass 
      [ T ProducerNameIntroduction
      , T BindNormalisation 
      ]

    -- cleanup after producer name intro
    runCleanup :: PipelineM () 
    runCleanup = void . pipelineStep $ Pass 
      [ T CopyPropagation 
      , T SimpleDeadVariableElimination
      ]

    needsCByLVA :: Transformation -> Bool
    needsCByLVA DeadCodeElimination = True 
    needsCByLVA _ = False 

    needsCleanup :: Transformation -> Bool 
    needsCleanup = needsCByLVA

confluenceTest :: PipelineM ()
confluenceTest = do
  pipelineLog "Confluence test"
  pipelineLog "Random pipeline #1"
  state <- MonadState.get
  gen1 <- liftIO newStdGen
  pipeline1 <- randomPipeline gen1
  pipelineLog "Random pipeline #2"
  exp1 <- use psExp
  MonadState.put state
  gen2 <- liftIO newStdGen
  pipeline2 <- randomPipeline gen2
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

<<<<<<< HEAD
runPipeline :: PipelineOpts -> Maybe String -> Exp -> PipelineM a -> IO (a, Exp)
runPipeline o s e m = fmap (second _psExp) $ flip runStateT start $ runReaderT m o where
  start = PState
    { _psSrc        = s
    , _psExp        = e
    , _psTransStep  = 0
    , _psSaveIdx    = 0
    , _psHPTProgram = Nothing
    , _psHPTResult  = Nothing
    , _psCByProgram = Nothing
    , _psCByResult  = Nothing
    , _psLVAProgram = Nothing
    , _psLVAResult  = Nothing
    , _psTypeEnv    = Nothing
    , _psTypeAnnots = Nothing
    , _psEffectMap  = Nothing
    , _psErrors     = []
    }
=======
debugPipelineState :: PipelineM ()
debugPipelineState = do
  ps <- MonadState.get
  liftIO $ print ps

runPipeline :: PipelineOpts -> Exp -> PipelineM a -> IO (a, Exp)
runPipeline o e m = do
  createDirectoryIfMissing True (_poOutputDir o)
  fmap (second _psExp) $ flip runStateT start $ runReaderT m o
  where
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
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70

-- | Runs the pipeline and returns the last version of the given
-- expression.
pipeline :: PipelineOpts -> Maybe String -> Exp -> [PipelineStep] -> IO ([(PipelineStep, PipelineEff)], Exp)
pipeline o s e ps = do
  print ps
  runPipeline o s e $ mapM (\p -> (,) p <$> pipelineStep p) ps

-- | Run the pipeline with the given set of transformations, till
-- it reaches a fixpoint where none of the pipeline transformations
-- change the expression itself, the order of the transformations
-- are defined in the pipeline list. When the expression changes,
-- it lints the resulting code, and performs a given sequence of 
-- pipeline steps on it. Finally, it performs a cleanup sequence
-- after each step.
optimizeWithPM :: PipelineOpts -> Exp -> [PipelineStep] -> [PipelineStep] -> [PipelineStep] -> PipelineM ()
optimizeWithPM o e ps onChange cleanUp = loop e where
  loop :: Exp -> PipelineM ()
  loop e = do
    -- Run every step and on changes run `onChange`
    effs <- forM ps $ \p -> do
      eff <- pipelineStep p
      when (eff == ExpChanged) $ void $ do
        pipelineStep $ SaveGrin (fmap (\case ' ' -> '-' ; c -> c) $ show p)
        lintGrin . Just $ show p
<<<<<<< HEAD
        mapM pipelineStep cleanUp
        mapM pipelineStep onChange
=======
        pipelineStep $ Eff CalcEffectMap
        when (o ^. poStatistics) $ void $ pipelineStep Statistics
        when (o ^. poSaveTypeEnv) $ void $ pipelineStep SaveTypeEnv
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70
      pure eff
    -- Run loop again on change
    pipelineStep $ PrintGrin id
    e' <- use psExp
    if mangleNames e == mangleNames e'
      then void $ mapM pipelineStep cleanUp
      else loop e'

optimize :: PipelineOpts -> Exp -> [PipelineStep] -> [PipelineStep] -> IO Exp
<<<<<<< HEAD
optimize o e pre post = optimizeWith o e pre defaultOptimizations defaultOnChange defaultCleanUp post where
=======
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
>>>>>>> 4a406cb3fd338669430d10b2fcc2e3876c672f70

optimizeWith :: PipelineOpts -> Exp -> [PipelineStep] -> [Transformation] -> [PipelineStep] -> [PipelineStep] -> [PipelineStep] -> IO Exp
optimizeWith o e pre optimizations onChange cleanUp post = fmap snd $ runPipeline o Nothing e $ do
  lintGrin $ Just "init"
  mapM_ pipelineStep pre
  mapM_ pipelineStep onChange
  optimizeWithPM o e (fmap T optimizations) onChange cleanUp
  mapM_ pipelineStep post
