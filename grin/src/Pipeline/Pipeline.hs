{-# LANGUAGE LambdaCase, RecordWildCards, RankNTypes #-}
module Pipeline.Pipeline
 ( module Pipeline.Pipeline
 , module Pipeline.Definitions
 , module Pipeline.Utils
 , emptyTypeEnv
 ) where

import Prelude
import Control.Monad
import Data.Text (Text)
import Data.Maybe (maybe, fromJust, fromMaybe)
import Text.Printf
import Text.Pretty.Simple (pPrint)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>), (<$$>))
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
import Grin.Pretty(showWide)
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
import Transformations.StaticSingleAssignment
import qualified Transformations.Simplifying.RightHoistFetch2 as RHF
import Transformations.Simplifying.RegisterIntroduction
import Transformations.Simplifying.ProducerNameIntroduction
import qualified AbstractInterpretation.HeapPointsTo.Result as HPT
import qualified AbstractInterpretation.CreatedBy.Readback as CBy
import qualified AbstractInterpretation.CreatedBy.Result as CBy
import qualified AbstractInterpretation.LiveVariable.Result as LVA
import qualified AbstractInterpretation.Sharing.Result as Sharing
import AbstractInterpretation.OptimiseAbstractProgram
import AbstractInterpretation.CreatedBy.Pretty
import AbstractInterpretation.HeapPointsTo.Pretty
import AbstractInterpretation.LiveVariable.Pretty
import AbstractInterpretation.Sharing.Pretty
import AbstractInterpretation.Sharing.CodeGen
import AbstractInterpretation.Reduce (ComputerState, AbstractInterpretationResult(..), evalDataFlowInfo)
import qualified AbstractInterpretation.PrettyIR as IR
import qualified AbstractInterpretation.IR as IR
import qualified AbstractInterpretation.HeapPointsTo.CodeGen as HPT
import qualified AbstractInterpretation.CreatedBy.CodeGen    as CBy
import qualified AbstractInterpretation.LiveVariable.CodeGen as LVA
import qualified AbstractInterpretation.Sharing.CodeGen      as Sharing
import qualified Reducer.LLVM.CodeGen as CGLLVM
import qualified Reducer.LLVM.JIT as JITLLVM
import System.Directory
import System.Process
import Data.Bifunctor

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import Data.Functor.Infix ((<$$>))
import Data.Maybe (isNothing)



data TransformationFunc
  = Plain          (Exp -> Exp)
  | WithTypeEnv    (TypeEnv -> Exp -> Either String Exp)
  | WithTypeEnvEff (TypeEnv -> EffectMap -> Exp -> Exp)
  | WithTypeEnvShr (Sharing.SharingResult -> TypeEnv -> Exp -> Exp)
  | WithLVA        (LVA.LVAResult -> TypeEnv -> Exp -> Either String Exp)
  | WithEffLVA     (LVA.LVAResult -> EffectMap -> TypeEnv -> Exp -> Either String Exp)
  | WithLVACBy     (LVA.LVAResult -> CBy.CByResult -> TypeEnv -> Exp -> Either String Exp)

-- TODO: Add n paramter for the transformations that use NameM
transformationFunc :: Int -> Transformation -> TransformationFunc
transformationFunc n = \case
  Vectorisation                   -> WithTypeEnv (Right <$$> Vectorisation2.vectorisation)
  GenerateEval                    -> Plain generateEval
  CaseSimplification              -> Plain caseSimplification
  SplitFetch                      -> Plain splitFetch
  RegisterIntroduction            -> Plain $ registerIntroductionI n
  ProducerNameIntroduction        -> Plain producerNameIntroduction
  RightHoistFetch                 -> Plain RHF.rightHoistFetch
  -- misc
  MangleNames                     -> Plain mangleNames
  StaticSingleAssignment          -> Plain staticSingleAssignment
  BindNormalisation               -> Plain bindNormalisation
  ConstantFolding                 -> Plain constantFolding
  -- optimising
  EvaluatedCaseElimination        -> Plain evaluatedCaseElimination
  TrivialCaseElimination          -> Plain trivialCaseElimination
  UpdateElimination               -> Plain updateElimination
  CopyPropagation                 -> Plain copyPropagation
  ConstantPropagation             -> Plain constantPropagation
  SimpleDeadFunctionElimination   -> Plain simpleDeadFunctionElimination
  SimpleDeadParameterElimination  -> Plain simpleDeadParameterElimination
  SimpleDeadVariableElimination   -> WithTypeEnvEff simpleDeadVariableElimination
  InlineEval                      -> WithTypeEnv (Right <$$> inlineEval)
  InlineApply                     -> WithTypeEnv (Right <$$> inlineApply)
  InlineBuiltins                  -> WithTypeEnv (Right <$$> inlineBuiltins)
  CommonSubExpressionElimination  -> WithTypeEnv (Right <$$> commonSubExpressionElimination)
  CaseCopyPropagation             -> Plain caseCopyPropagation
  CaseHoisting                    -> WithTypeEnv (Right <$$> caseHoisting)
  GeneralizedUnboxing             -> WithTypeEnv (Right <$$> generalizedUnboxing)
  ArityRaising                    -> WithTypeEnv (Right <$$> (arityRaising n))
  LateInlining                    -> WithTypeEnv (Right <$$> lateInlining)
  UnitPropagation                 -> WithTypeEnv (Right <$$> unitPropagation)
  NonSharedElimination            -> WithTypeEnvShr nonSharedElimination
  DeadFunctionElimination         -> WithEffLVA deadFunctionElimination
  DeadVariableElimination         -> WithEffLVA deadVariableElimination
  DeadParameterElimination        -> WithLVA deadParameterElimination
  DeadDataElimination             -> WithLVACBy deadDataElimination
  SparseCaseOptimisation          -> WithTypeEnv sparseCaseOptimisation

transformation :: RunAnalysis -> Transformation -> PipelineM ()
transformation runAnalysis t = do
  when runAnalysis $ runAnalysisFor t
  n <- use psTransStep
  e <- use psExp
  te <- fromMaybe (traceShow "empty type env is used" emptyTypeEnv) <$> use psTypeEnv
  em <- fromMaybe (traceShow "empty effect map is used" mempty) <$> use psEffectMap
  cby <- fromMaybe (traceShow "empty created by result is used" CBy.emptyCByResult) <$> use psCByResult
  lva <- fromMaybe (traceShow "empty live variable result is used" LVA.emptyLVAResult) <$> use psLVAResult
  shr <- fromMaybe (traceShow "empty sharing result is used" Sharing.emptySharingResult) <$> use psSharingResult
  either (\err -> psErrors %= (err:)) (psExp .=) $
    case transformationFunc n t of
      Plain          f -> Right $ f e
      WithTypeEnv    f -> f te e
      WithTypeEnvEff f -> Right $ f te em e
      WithLVA        f -> f lva te e
      WithEffLVA     f -> f lva em te e
      WithLVACBy     f -> f lva cby te e
      WithTypeEnvShr f -> Right $ f shr te e
  psTransStep %= (+1)

pipelineStep :: PipelineStep -> PipelineM PipelineEff
pipelineStep p = do
  case p of
    Pass{}               -> pure () -- each pass step will be printed anyway
    _ | isPrintingStep p -> pipelineLog $ printf "PipelineStep: %-35s" (show p)
    _                    -> pipelineLogNoLn $ printf "PipelineStep: %-35s" (show p)
  before <- use psExp
  start <- liftIO getCurrentTime
  case p of
    Optimize -> optimizeWithM [] defaultOptimizations []
    HPT step -> case step of
      Compile -> compileAbstractProgram HPT.codeGen psHPTProgram
      Optimise  -> optimiseAbsProgWith psHPTProgram "HPT program is not available to be optimized"
      PrintProgram     -> printAbstractProgram psHPTProgram
      RunPure   -> runHPTPure
      PrintResult      -> printAnalysisResult psHPTResult
    CBy step -> case step of
      Compile -> compileAbstractProgram CBy.codeGen psCByProgram
      Optimise  -> optimiseAbsProgWith psCByProgram "CBy program is not available to be optimized"
      PrintProgram     -> printAbstractProgram psCByProgram
      RunPure   -> runCByPure
      PrintResult      -> printAnalysisResult psCByResult
    LVA step -> case step of
      Compile -> compileAbstractProgram LVA.codeGen psLVAProgram
      Optimise  -> optimiseAbsProgWith psLVAProgram "LVA program is not available to be optimized"
      PrintProgram     -> printAbstractProgram psLVAProgram
      RunPure   -> runLVAPure
      PrintResult      -> printAnalysisResult psLVAResult
    RunCByWithLVA -> runCByWithLVAPure
    Sharing step -> case step of
      Compile -> compileAbstractProgram Sharing.codeGen psSharingProgram
      Optimise  -> optimiseAbsProgWith psSharingProgram "Sharing program is not available to be optimized"
      PrintProgram     -> printAbstractProgram psSharingProgram
      RunPure   -> runSharingPure
      PrintResult      -> printAnalysisResult psSharingResult
    Eff eff -> case eff of
      CalcEffectMap   -> calcEffectMap
      PrintEffectMap  -> printEffectMap
    T r t             -> transformation r t
    Pass pass       -> mapM_ pipelineStep pass
    PrintGrin d     -> printGrinM d
    PureEval        -> pureEval
    JITLLVM         -> jitLLVM
    SaveLLVM relPath path -> saveLLVM relPath path
    SaveGrin path   -> saveGrin path
    PrintAST        -> printAST
    PrintTypeAnnots -> printTypeAnnots
    PrintTypeEnv    -> printTypeEnv
    SaveTypeEnv     -> saveTypeEnv
    DebugTransformation t -> debugTransformation t
    Statistics      -> statistics
    Lint            -> lintGrin Nothing
    ConfluenceTest  -> confluenceTest
    PrintErrors     -> do
      errors <- use psErrors
      pipelineLog $ unlines $ "errors:" : errors
    DebugPipelineState -> debugPipelineState
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

optimiseAbsProgWith :: IR.HasDataFlowInfo a => Lens' PState (Maybe a) -> String -> PipelineM ()
optimiseAbsProgWith getProg err = do
  mProg <- use getProg
  case mProg of
    Just prog -> getProg._Just %= IR.modifyInfo optimiseAbstractProgram
    Nothing   -> pipelineLog err

compileAbstractProgram :: (Exp -> Either String prog) -> (Lens' PState (Maybe prog)) -> PipelineM ()
compileAbstractProgram codeGen accessProg = do
  grin <- use psExp
  case codeGen grin of
    Right absProg ->
      accessProg .= Just absProg
    Left e -> do
      psErrors %= (e:)
      accessProg .= Nothing

printAbsProg :: IR.AbstractProgram -> PipelineM ()
printAbsProg a = do
  pipelineLog $ show $ IR.prettyInstructions (Just a) . IR._absInstructions $ a
  pipelineLog $ printf "memory size    %d" $ IR._absMemoryCounter a
  pipelineLog $ printf "register count %d" $ IR._absRegisterCounter a
  pipelineLog $ printf "variable count %d" $ Map.size $ IR._absRegisterMap a

printAbstractProgram :: IR.HasDataFlowInfo a => (Lens' PState (Maybe a)) -> PipelineM ()
printAbstractProgram accessProg = do
  progM <- use accessProg
  mapM_ (printAbsProg . IR.getDataFlowInfo) progM

printAnalysisResult :: Pretty res => (Lens' PState (Maybe res)) -> PipelineM ()
printAnalysisResult accessRes = use accessRes >>= \case
  Nothing -> pure ()
  Just result -> pipelineLog $ show $ pretty result


runHPTPure :: PipelineM ()
runHPTPure = use psHPTProgram >>= \case
  Nothing -> psHPTResult .= Nothing
  Just hptProgram -> do
    let AbsIntResult{..} = evalDataFlowInfo hptProgram
        result = HPT.toHPTResult hptProgram _airComp
    pipelineLogIterations _airIter
    psHPTResult .= Just result
    case typeEnvFromHPTResult result of
      Right te  -> psTypeEnv .= Just te
      Left err  -> do
        psErrors %= (err :)
        liftIO $ printf "type-env error: %s" err
        psTypeEnv .= Nothing


runCByPureWith :: (CBy.CByProgram -> ComputerState -> CBy.CByResult) -> PipelineM ()
runCByPureWith toCByResult = use psCByProgram >>= \case
  Nothing -> psCByResult .= Nothing
  Just cbyProgram -> do
    let AbsIntResult{..} = evalDataFlowInfo cbyProgram
        result = toCByResult cbyProgram _airComp
    pipelineLogIterations _airIter
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


runLVAPure :: PipelineM ()
runLVAPure = use psLVAProgram >>= \case
  Nothing -> psLVAResult .= Nothing
  Just lvaProgram -> do
    let AbsIntResult{..} = evalDataFlowInfo lvaProgram
        result = LVA.toLVAResult lvaProgram _airComp
    pipelineLogIterations _airIter
    psLVAResult .= Just result

runSharingPureWith :: (Sharing.SharingProgram -> ComputerState -> Sharing.SharingResult) -> PipelineM ()
runSharingPureWith toSharingResult = use psSharingProgram >>= \case
  Nothing -> psSharingResult .= Nothing
  Just shProgram -> do
    let AbsIntResult{..} = evalDataFlowInfo shProgram
        result = toSharingResult shProgram _airComp
    pipelineLogIterations _airIter
    psSharingResult .= Just result
    case typeEnvFromHPTResult (Sharing._hptResult result) of
      Right te  -> psTypeEnv .= Just te
      Left err  -> do
        psErrors %= (err :)
        psTypeEnv .= Nothing

runSharingPure :: PipelineM ()
runSharingPure = runSharingPureWith Sharing.toSharingResult


printTypeAnnots :: PipelineM ()
printTypeAnnots = do
  typeEnv <- use psTypeAnnots
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
    createDirectoryIfMissing True outputDir
    writeFile (outputDir </> fname) $ showWide $ plain $ pretty content

saveTypeEnv :: PipelineM ()
saveTypeEnv = do
  mTypeEnv <- use psTypeEnv
  forM_ mTypeEnv $ saveTransformationInfo "Type-Env"
  mHPTResult <- use psHPTResult
  forM_ mHPTResult $ saveTransformationInfo "HPT-Result"

statistics :: PipelineM ()
statistics = do
  exp <- use psExp
  saveTransformationInfo "Statistics" $ Statistics.statistics exp

pureEval :: PipelineM ()
pureEval = do
  e <- use psExp
  val <- liftIO $ evalProgram PureReducer e
  pipelineLog $ show $ pretty val

printGrinM :: (Doc -> Doc) -> PipelineM ()
printGrinM color = do
  e <- use psExp
  pipelineLog $ showWide $ color $ pretty e

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

saveGrin :: Path -> PipelineM ()
saveGrin path = do
  psSaveIdx %= succ
  e <- use psExp
  case path of
    Rel fn -> saveTransformationInfo fn e
    Abs fn -> liftIO $ do
      writeFile fn $ show $ plain $ pretty e

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
  pipelineStep $ HPT Compile
  pipelineStep $ HPT RunPure
  exp <- use psExp
  mTypeEnv <- use psTypeEnv
  let lintExp@(_, errorMap) = Lint.lint mTypeEnv exp
  psErrors .= (fmap message $ concat $ Map.elems errorMap)

  -- print errors
  errors <- use psErrors
  unless (Prelude.null errors) $ void $ do
    failOnLintError <- view poFailOnLint
    when failOnLintError $ void $ do
      pipelineLog $ show $ prettyLintExp lintExp
      pipelineStep $ HPT PrintResult
    case mPhaseName of
      Just phaseName  -> pipelineLog $ printf "error after %s:\n%s" phaseName (unlines errors)
      Nothing         -> pipelineLog $ printf "error:\n%s" (unlines errors)
    saveTransformationInfo "Lint" $ prettyLintExp lintExp
    mHptResult <- use psHPTResult
    saveTransformationInfo "HPT-Result" mHptResult
    when failOnLintError $ do
      -- FIXME: reenable after: undefined support ; transformation to inject default alts for pattern match errors
      -- liftIO $ die "illegal code"
      pure ()

-- confluence testing

-- Generate random pipeline based on the transformationWhitelist, the pipeline reaches a fixpoint
-- and returns the list of transformation that helped to reach the fixpoint.
randomPipeline :: StdGen -> PipelineM [Transformation]
randomPipeline seed = do
  liftIO $ setStdGen seed
  runBasicAnalyses
  go transformationWhitelist []
  where
    go :: [Transformation] -> [Transformation] -> PipelineM [Transformation]
    go [] result = do
      -- The final result must be normalised as, non-normalised and normalised
      -- grin program is semantically the same.
      pipelineStep $ T RunAnalysis BindNormalisation
      pure $ reverse result
    go available res = do
      exp <- use psExp
      t <- fmap ((available !!) . abs . (`mod` (length available))) $ liftIO $ randomIO
      eff <- if needsCByLVA t
        then do
          runNameIntro
          runCByLVA
          pipelineStep (T RunAnalysis t)
          runCleanup
          exp' <- use psExp
          pure $ if exp == exp' then None else ExpChanged
        else pipelineStep (T RunAnalysis t)
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
        , NonSharedElimination
        , CopyPropagation
        , ConstantPropagation
        , SimpleDeadFunctionElimination
        , SimpleDeadParameterElimination
        , SimpleDeadVariableElimination
        , DeadFunctionElimination
        , DeadDataElimination
        , DeadVariableElimination
        , DeadParameterElimination
        , CommonSubExpressionElimination
        , CaseCopyPropagation
        , CaseHoisting
        , GeneralizedUnboxing
        , ArityRaising
        , LateInlining
        ]

    runBasicAnalyses :: PipelineM ()
    runBasicAnalyses = mapM_ pipelineStep
      [ Sharing Compile
      , Sharing RunPure
      , Eff CalcEffectMap
      ]

    runCByLVA :: PipelineM ()
    runCByLVA = mapM_ pipelineStep
      [ CBy Compile
      , CBy RunPure
      , LVA Compile
      , LVA RunPure
      , Eff CalcEffectMap
      ]

    runNameIntro :: PipelineM ()
    runNameIntro = void . pipelineStep $ Pass
      [ T RunAnalysis ProducerNameIntroduction
      , T RunAnalysis BindNormalisation
      ]

    -- cleanup after producer name intro
    runCleanup :: PipelineM ()
    runCleanup = void . pipelineStep $ Pass
      [ T RunAnalysis CopyPropagation
      , T RunAnalysis SimpleDeadVariableElimination
      ]

    needsCByLVA :: Transformation -> Bool
    needsCByLVA = \case
      DeadFunctionElimination -> True
      DeadDataElimination -> True
      DeadVariableElimination -> True
      DeadParameterElimination -> True
      _ -> False

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

runPipeline :: PipelineOpts -> TypeEnv -> Exp -> PipelineM a -> IO (a, Exp)
runPipeline o ta e m = do
  createDirectoryIfMissing True $ _poOutputDir o
  fmap (second _psExp) $ flip runStateT start $ runReaderT m o where
    start = PState
      { _psExp            = e
      , _psTransStep      = 0
      , _psSaveIdx        = 0
      , _psHPTProgram     = Nothing
      , _psHPTResult      = Nothing
      , _psCByProgram     = Nothing
      , _psCByResult      = Nothing
      , _psLVAProgram     = Nothing
      , _psLVAResult      = Nothing
      , _psSharingResult  = Nothing
      , _psSharingProgram = Nothing
      , _psTypeEnv        = Nothing
      , _psTypeAnnots     = ta
      , _psEffectMap      = Nothing
      , _psErrors         = []
      }

-- | Runs the pipeline and returns the last version of the given
-- expression.
pipeline :: PipelineOpts -> TypeEnv -> Exp -> [PipelineStep] -> IO ([(PipelineStep, PipelineEff)], Exp)
pipeline o ta e ps = do
  runPipeline o ta e $ mapM (\p -> (,) p <$> pipelineStep p) ps

optimize :: PipelineOpts -> Exp -> [PipelineStep] -> [PipelineStep] -> IO Exp
optimize o e pre post = optimizeWith o e pre defaultOptimizations post

optimizeWith :: PipelineOpts -> Exp -> [PipelineStep] -> [Transformation] -> [PipelineStep] -> IO Exp
optimizeWith o e pre ts post =
  fmap snd $ runPipeline o emptyTypeEnv e $ optimizeWithM pre ts post

-- | Run the pipeline with the given set of transformations, till
-- it reaches a fixpoint where none of the pipeline transformations
-- change the expression itself, the order of the transformations
-- are defined in the pipeline list. When the expression changes,
-- it lints the resulting code.
--
-- phase optimisation
-- - loop over transformations while the expression changes in one phase
-- - bump to the next phase when the expression did not change
-- - phases are ordered via complexity
-- - the expression reaches a fixpoint when none of the phases did change the expression
optimizeWithM :: [PipelineStep] -> [Transformation] -> [PipelineStep] -> PipelineM ()
optimizeWithM pre trans post = do
  mapM_ pipelineStep pre
  loop
  mapM_ pipelineStep post
  where
    loop = do
      pipelineLog "PHASE #1"
      c1 <- phase1
      pipelineLog "PHASE #2"
      c2 <- phase2
      pipelineLog "PHASE #3"
      c3 <- phase3
      pipelineLog "PHASE #4"
      c4 <- phase4
      when (or [c1, c2, c3, c4]) loop

    phaseLoop isChanged ts = do
      o <- ask
      pipelineStep (T RunAnalysis BindNormalisation)
      effs <- forM ts $ \t -> do
        eff <- pipelineStep (T RunAnalysis t)
        when (eff == ExpChanged) $ do
          pipelineStep $ SaveGrin $ Rel $ (fmap (\case ' ' -> '-' ; c -> c) $ show t) <.> "grin"
          when (o ^. poLintOnChange) $ lintGrin $ Just $ show t
          when (o ^. poStatistics)  $ void $ pipelineStep Statistics
          when (o ^. poSaveTypeEnv) $ void $ pipelineStep SaveTypeEnv
          invalidateAnalysisResults
        pure eff
      if (any (==ExpChanged) effs)
        then phaseLoop True ts
        else pure isChanged

    -- No analysis is required
    phase1 = phaseLoop False $ trans `intersect`
      [ BindNormalisation
      , EvaluatedCaseElimination
      , TrivialCaseElimination
      , UpdateElimination
      , CopyPropagation
      , ConstantPropagation
      , SimpleDeadFunctionElimination
      , SimpleDeadParameterElimination
      , CaseCopyPropagation
      ]

    -- HPT is required
    phase2 = phaseLoop False $ trans `intersect`
      [ InlineEval
      , InlineApply
      , InlineBuiltins
      , CommonSubExpressionElimination
      , CaseHoisting
      , GeneralizedUnboxing
      , ArityRaising
      , LateInlining
      , UnitPropagation
      , SparseCaseOptimisation
      ]

    -- HPT and Sharing/Eff is required
    phase3 = phaseLoop False $ trans `intersect`
      [ SimpleDeadVariableElimination
      , NonSharedElimination
      ]

    -- HPT LVA CBy is required
    -- Only run this phase when interprocedural transformations are required.
    phase4 = if (null (trans `intersect`
                  [ DeadDataElimination
                  , DeadFunctionElimination
                  , DeadParameterElimination
                  , DeadVariableElimination
                  ]))
      then pure False
      else phase4Loop False

    phase4Loop isChanged = do
      o <- ask
      expBefore <- use psExp
      forM_ steps $ \step -> do
        eff <- pipelineStep step
        when (eff == ExpChanged) $ void $ do
          pipelineStep $ SaveGrin $ Rel $ (fmap (\case ' ' -> '-' ; c -> c) $ show step) <.> "grin"
          when (o ^. poLintOnChange) $ lintGrin $ Just $ show step
          when (o ^. poStatistics)  $ void $ pipelineStep Statistics
          when (o ^. poSaveTypeEnv) $ void $ pipelineStep SaveTypeEnv
      expAfter <- use psExp
      if (mangleNames expBefore /= mangleNames expAfter)
        then phase4Loop True
        else pure isChanged
      where
        steps = concat
          [ map (T RunAnalysis)
              [ CopyPropagation
              , SimpleDeadVariableElimination
              , ProducerNameIntroduction
              , BindNormalisation
              , UnitPropagation
              ]
          , [ Pass $ map HPT [Compile, RunPure]
            , Pass $ map CBy [Compile, RunPure]
            , Pass $ map LVA [Compile, RunPure]
            , Pass $ map Sharing [Compile, RunPure]
            , Eff CalcEffectMap
            ]
          , map (T DoNotRunAnalysis) $ trans `intersect`
              [ DeadFunctionElimination
              , DeadDataElimination
              , DeadVariableElimination
              , DeadParameterElimination
              ]
          , map (T RunAnalysis)
              [ CopyPropagation
              , SimpleDeadVariableElimination
              , BindNormalisation
              , UnitPropagation
              ]
          ]

invalidateAnalysisResults :: PipelineM ()
invalidateAnalysisResults = do
  psHPTProgram     .= Nothing
  psHPTResult      .= Nothing
  psCByProgram     .= Nothing
  psCByResult      .= Nothing
  psLVAProgram     .= Nothing
  psLVAResult      .= Nothing
  psSharingProgram .= Nothing
  psSharingResult  .= Nothing
  psTypeEnv        .= Nothing
  psEffectMap      .= Nothing

runAnalysisFor :: Transformation -> PipelineM ()
runAnalysisFor t = do
  n <- use psTransStep
  sequence_ $ case transformationFunc n t of
    Plain          _ -> []
    WithTypeEnv    _ -> [hpt]
    WithTypeEnvEff _ -> [hpt, eff]
    WithLVA        _ -> [hpt, lva]
    WithEffLVA     _ -> [hpt, lva, eff]
    WithLVACBy     _ -> [hpt, lva, cby]
    WithTypeEnvShr _ -> [hpt, sharing]
  where
    analisys getter ann = do
      r <- use getter
      when (isNothing r) $ do
        pipelineLog $ "Analisys"
        mapM_ pipelineStep $ (ann <$> [Compile, RunPure])

    hpt = analisys psHPTResult HPT
    lva = analisys psLVAResult LVA
    cby = analisys psCByResult CBy
    sharing = analisys psSharingResult Sharing

    eff :: PipelineM ()
    eff = do
      r <- use psEffectMap
      when (isNothing r) $ do
        pipelineLog $ "Analisys"
        void $ pipelineStep $ Eff CalcEffectMap
