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
import AbstractInterpretation.Reduce (Computer, AbstractInterpretationResult(..), evalDataFlowInfo)
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
  StaticSingleAssignment          -> noEffectMap $ noTypeEnv staticSingleAssignment
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
  ArityRaising                    -> noEffectMap (arityRaising n)
  LateInlining                    -> noEffectMap lateInlining
  UnitPropagation                 -> noEffectMap unitPropagation

pipelineStep :: PipelineStep -> PipelineM PipelineEff
pipelineStep p = do
  case p of
    Pass{}               -> pure () -- each pass step will be printed anyway
    _ | isPrintingStep p -> pipelineLog $ printf "PipelineStep: %-35s" (show p)
    _                    -> pipelineLogNoLn $ printf "PipelineStep: %-35s" (show p)
  before <- use psExp
  start <- liftIO getCurrentTime
  case p of
    Optimize -> do
      mapM_ pipelineStep defaultOnChange
      optimizeWithPM (T <$> defaultOptimizations) defaultOnChange defaultCleanUp
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
    T t             -> transformationM t
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


runCByPureWith :: (CBy.CByProgram -> Computer -> CBy.CByResult) -> PipelineM ()
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


runSharingPureWith :: (Sharing.SharingProgram -> Computer -> Sharing.SharingResult) -> PipelineM ()
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
    writeFile (outputDir </> fname) $ show $ plain $ pretty content

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

transformationM :: Transformation -> PipelineM ()
transformationM NonSharedElimination = do
  e <- use psExp
  withTyEnvSharing $ \tyEnv shRes -> do
    let e' = nonSharedElimination shRes tyEnv e
    psExp .= e'
    psTransStep %= (+1)


transformationM DeadCodeElimination = do
  withEffMapTyEnvCByLVA $ \effMap typeEnv cbyResult lvaResult -> do

    e <- use psExp
    case deadFunctionElimination lvaResult effMap typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

    e  <- use psExp
    case deadDataElimination lvaResult cbyResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

    e <- use psExp
    case deadVariableElimination lvaResult effMap typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

    e <- use psExp
    case deadParameterElimination lvaResult typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

transformationM DeadFunctionElimination = do
  e  <- use psExp
  withEffMapTyEnvLVA $ \effMap typeEnv lvaResult -> do
    case deadFunctionElimination lvaResult effMap typeEnv e of
      Right e'  -> psExp .= e' >> psTransStep %= (+1)
      Left  err -> psErrors %= (err:)

transformationM DeadVariableElimination = do
  e  <- use psExp
  withEffMapTyEnvLVA $ \effMap typeEnv lvaResult -> do
    case deadVariableElimination lvaResult effMap typeEnv e of
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
  env0 <- fromMaybe (traceShow "emptyTypeEnv is used" emptyTypeEnv) <$> use psTypeEnv
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
      pipelineStep $ T BindNormalisation
      pure $ reverse result
    go available res = do
      exp <- use psExp
      t <- fmap ((available !!) . abs . (`mod` (length available))) $ liftIO $ randomIO
      eff <- if needsCByLVA t
        then do
          runNameIntro
          runCByLVA
          pipelineStep (T t)
          runCleanup
          exp' <- use psExp
          pure $ if exp == exp' then None else ExpChanged
        else pipelineStep (T t)
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
        , DeadCodeElimination
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

-- | Run the pipeline with the given set of transformations, till
-- it reaches a fixpoint where none of the pipeline transformations
-- change the expression itself, the order of the transformations
-- are defined in the pipeline list. When the expression changes,
-- it lints the resulting code, and performs a given sequence of
-- pipeline steps on it. Finally, it performs a cleanup sequence
-- after each step.
-- TODO: Remove options parameter as it should be read from the PipelineM
optimizeWithPM :: [PipelineStep] -> [PipelineStep] -> [PipelineStep] -> PipelineM ()
optimizeWithPM ps onChange cleanUp = loop where
  loop :: PipelineM ()
  loop = do
    -- Run every step and on changes run `onChange`
    e <- use psExp
    effs <- forM ps $ \p -> do
      eff <- pipelineStep p
      when (eff == ExpChanged) $ void $ do
        pipelineStep $ SaveGrin $ Rel $ fmap (\case ' ' -> '-' ; c -> c) $ show p
        lintGrin . Just $ show p
        mapM_ pipelineStep cleanUp
        mapM_ pipelineStep onChange
      pure eff
    -- Run loop again on change
    o <- ask
    when (o ^. poStatistics)  $ void $ pipelineStep Statistics
    when (o ^. poSaveTypeEnv) $ void $ pipelineStep SaveTypeEnv
    e' <- use psExp
    if (any (==ExpChanged) effs)
      then loop
      else mapM_ pipelineStep cleanUp

optimize :: PipelineOpts -> Exp -> [PipelineStep] -> [PipelineStep] -> IO Exp
optimize o e pre post = optimizeWith o e pre defaultOptimizations defaultOnChange defaultCleanUp post where

optimizeWith
  :: PipelineOpts
  -> Exp
  -> [PipelineStep]   -- ^ Pre optimisation steps
  -> [Transformation] -- ^ Selected transformations for the optimisation
  -> [PipelineStep]   -- ^ Steps to run when transformation changed the program
  -> [PipelineStep]   -- ^ Steps to run on clean-up after a change. -- TODO: Why this necessary? Transformations should be well-contained
  -> [PipelineStep]   -- ^ Steps to run after a reached fixpoint
  -> IO Exp
optimizeWith o e pre optimizations onChange cleanUp post = fmap snd $ runPipeline o emptyTypeEnv e $ do
  lintGrin $ Just "init"
  mapM_ pipelineStep pre
  mapM_ pipelineStep onChange
  optimizeWithPM (T <$> optimizations) onChange cleanUp
  mapM_ pipelineStep post
