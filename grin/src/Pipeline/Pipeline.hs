{-# LANGUAGE LambdaCase, RecordWildCards, RankNTypes, PatternSynonyms, TemplateHaskell #-}
module Pipeline.Pipeline
  ( PipelineOpts(..)
  , defaultOpts
  , PipelineStep(..)
  , AbstractComputationStep(..)
  , Transformation(..)
  , EffectStep(..)
  , Path(..)
  , RenderingOption(..)
  , pattern HPTPass
  , pattern PrintGrin
  , pattern SimplePrintGrin
  , pattern FullPrintGrin
  , pattern DeadCodeElimination
  , pattern PureEvalPlugin
  , pattern DefinitionalInterpreter
  , pipeline
  , optimize
  , optimizeWith
  , randomPipeline
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
import Grin.Grin
import Grin.TypeEnv
import Grin.TypeCheck
import Grin.EffectMap hiding (Eff)
import Pipeline.Optimizations
import qualified Grin.Statistics as Statistics
import Grin.Parse
import Grin.Pretty(showWide, prettyProgram, RenderingOption(..))
import Transformations.CountVariableUse
import Transformations.GenerateEval
import qualified Transformations.Simplifying.Vectorisation2 as Vectorisation2
import Transformations.Simplifying.Vectorisation
import Transformations.BindNormalisation
import qualified Grin.Lint as Lint
import Grin.PrettyLint
import Transformations.Simplifying.SplitFetch
import Transformations.Simplifying.BindingPatternSimplification
import Transformations.Simplifying.CaseSimplification
import Transformations.Optimising.Inlining (inlineEval, inlineApply, inlineBuiltins)
import Transformations.UnitPropagation
import Transformations.MangleNames
import Transformations.EffectMap
import Transformations.StaticSingleAssignment
import Transformations.Names (ExpChanges(..))
import qualified Transformations.Simplifying.RightHoistFetch2 as RHF
import Transformations.Simplifying.RegisterIntroduction
import Transformations.Simplifying.ProducerNameIntroduction
import qualified AbstractInterpretation.HeapPointsTo.Result   as HPT
import qualified AbstractInterpretation.CreatedBy.Readback    as CBy
import qualified AbstractInterpretation.CreatedBy.Result      as CBy
import qualified AbstractInterpretation.LiveVariable.Result   as LVA
import qualified AbstractInterpretation.EffectTracking.Result as ET
import qualified AbstractInterpretation.Sharing.Result        as Sharing
import AbstractInterpretation.BinaryIR
import AbstractInterpretation.OptimiseAbstractProgram
import AbstractInterpretation.CreatedBy.Pretty
import AbstractInterpretation.HeapPointsTo.Pretty
import AbstractInterpretation.LiveVariable.Pretty
import AbstractInterpretation.EffectTracking.Pretty
import AbstractInterpretation.Sharing.Pretty
import AbstractInterpretation.Sharing.CodeGen
import AbstractInterpretation.Reduce (ComputerState, AbstractInterpretationResult(..), evalAbstractProgram)
import qualified AbstractInterpretation.PrettyIR as IR
import qualified AbstractInterpretation.IR as IR
import qualified AbstractInterpretation.HeapPointsTo.CodeGen         as HPT
import qualified AbstractInterpretation.HeapPointsTo.CodeGenBase     as HPT
import qualified AbstractInterpretation.CreatedBy.CodeGen            as CBy
import qualified AbstractInterpretation.LiveVariable.CodeGen         as LVA
import qualified AbstractInterpretation.EffectTracking.CodeGen       as ET
import qualified AbstractInterpretation.EffectTracking.CodeGenBase   as ET
import qualified AbstractInterpretation.Sharing.CodeGen              as Sharing
import qualified Reducer.LLVM.CodeGen as CGLLVM
import qualified Reducer.LLVM.JIT as JITLLVM
import System.Environment ( lookupEnv )
import System.Directory
import qualified System.Process
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

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Control.Monad.Extra
import System.Random
import Data.Time.Clock
import Data.Fixed
import Data.Functor.Infix
import Data.Maybe (isNothing)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Data.Binary as Binary
import Grin.Nametable as Nametable
import qualified Data.ByteString.Lazy as LBS
import Reducer.PrimOps (evalPrimOp)
import Reducer.Pure (EvalPlugin(..))

data Transformation
  -- Simplifying
  = RegisterIntroduction
  | ProducerNameIntroduction
  | BindingPatternSimplification
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
  | StaticSingleAssignment
  -- Optimizations
  | EvaluatedCaseElimination
  | TrivialCaseElimination
  | SparseCaseOptimisation
  | UpdateElimination
  | NonSharedElimination
  | CopyPropagation
  | ConstantPropagation
  | DeadDataElimination
  | DeadFunctionElimination
  | DeadParameterElimination
  | DeadVariableElimination
  | SimpleDeadFunctionElimination
  | SimpleDeadVariableElimination
  | SimpleDeadParameterElimination
  | CommonSubExpressionElimination
  | CaseCopyPropagation
  | CaseHoisting
  | GeneralizedUnboxing
  | ArityRaising
  | LateInlining
  deriving (Enum, Eq, Ord, Show)

newtype Hidden a = H a

instance Show (Hidden a) where
  show _ = "(hidden)"

instance Eq (Hidden a) where
  _ == _ = True

data AbstractComputationStep
  = Compile
  | Optimise
  | PrintProgram
  | SaveProgram String
  | RunPure
  | PrintResult
  deriving (Eq, Show)

data EffectStep
  = CalcEffectMap
  | PrintEffectMap
  deriving (Eq, Show)

data PipelineStep
  = Optimize
  | HPT AbstractComputationStep
  | CBy AbstractComputationStep
  | LVA AbstractComputationStep
  | ET  AbstractComputationStep
  | Sharing AbstractComputationStep
  | RunCByWithLVA -- TODO: Remove
  | Eff EffectStep
  | T Transformation
  | Pass [PipelineStep]
  | PrintGrinH RenderingOption (Hidden (Doc -> Doc))
  | PureEval Bool
  | PureEvalPluginH (Hidden EvalPlugin) Bool
  | DefinitionalInterpreterH (Hidden EvalPlugin) Bool
  | JITLLVM
  | PrintAST
  | SaveLLVM Path
  | SaveExecutable Bool Path -- Debug, Outputfile
  | SaveGrin Path
  | SaveBinary String
  | DebugTransformationH (Hidden (Exp -> Exp))
  | Statistics
  | PrintTypeAnnots
  | PrintTypeEnv
  | SaveTypeEnv
  | Lint
  | ConfluenceTest
  | PrintErrors
  | DebugPipelineState
  deriving (Eq, Show)

pattern DeadCodeElimination :: PipelineStep
pattern DeadCodeElimination = Pass
  [ T DeadFunctionElimination
  , T DeadDataElimination
  , T DeadVariableElimination
  , T DeadParameterElimination
  ]

pattern HPTPass :: PipelineStep
pattern HPTPass = Pass
  [ HPT Compile
  , HPT RunPure
  ]

data Path
  = Abs FilePath
  | Rel FilePath
  deriving (Eq, Show)

pattern PrintGrin :: RenderingOption -> (Doc -> Doc) -> PipelineStep
pattern PrintGrin r c <- PrintGrinH r (H c)
  where PrintGrin r c =  PrintGrinH r (H c)

pattern SimplePrintGrin :: (Doc -> Doc) -> PipelineStep
pattern SimplePrintGrin c <- PrintGrinH Simple (H c)
  where SimplePrintGrin c =  PrintGrinH Simple (H c)

pattern FullPrintGrin :: (Doc -> Doc) -> PipelineStep
pattern FullPrintGrin c <- PrintGrinH WithExternals (H c)
  where FullPrintGrin c =  PrintGrinH WithExternals (H c)

pattern DebugTransformation :: (Exp -> Exp) -> PipelineStep
pattern DebugTransformation t <- DebugTransformationH (H t)
  where DebugTransformation t =  DebugTransformationH (H t)

pattern PureEvalPlugin :: EvalPlugin -> Bool -> PipelineStep
pattern PureEvalPlugin t b <- PureEvalPluginH (H t) b
  where PureEvalPlugin t b =  PureEvalPluginH (H t) b

pattern DefinitionalInterpreter :: EvalPlugin -> Bool -> PipelineStep
pattern DefinitionalInterpreter t b <- DefinitionalInterpreterH (H t) b
  where DefinitionalInterpreter t b =  DefinitionalInterpreterH (H t) b

data PipelineOpts = PipelineOpts
  { _poOutputDir   :: FilePath
  , _poFailOnLint  :: Bool
  , _poLogging     :: Bool
  , _poSaveTypeEnv :: Bool
  , _poStatistics  :: Bool
  , _poLintOnChange :: Bool
  , _poTypedLint :: Bool -- Run HPT before every lint
  , _poSaveBinary :: Bool
  , _poCFiles :: [FilePath]
  }

defaultOpts :: PipelineOpts
defaultOpts = PipelineOpts
  { _poOutputDir    = ".grin-output"
  , _poFailOnLint   = True
  , _poLogging      = True
  , _poSaveTypeEnv  = False
  , _poStatistics   = False
  , _poLintOnChange = True
  , _poTypedLint    = False
  , _poSaveBinary   = False
  , _poCFiles       = []
  }

type PipelineM a = ReaderT PipelineOpts (StateT PState IO) a
data PState = PState
    { _psExp            :: Exp
    , _psTransStep      :: !Int
    , _psSaveIdx        :: !Int
    , _psHPTProgram     :: Maybe (IR.AbstractProgram, HPT.HPTMapping)
    , _psHPTResult      :: Maybe HPT.HPTResult
    , _psCByProgram     :: Maybe (IR.AbstractProgram, CBy.CByMapping)
    , _psCByResult      :: Maybe CBy.CByResult
    , _psLVAProgram     :: Maybe (IR.AbstractProgram, LVA.LVAMapping)
    , _psLVAResult      :: Maybe LVA.LVAResult
    , _psETProgram      :: Maybe (IR.AbstractProgram, ET.ETMapping)
    , _psETResult       :: Maybe ET.ETResult
    , _psSharingProgram :: Maybe (IR.AbstractProgram, Sharing.SharingMapping)
    , _psSharingResult  :: Maybe Sharing.SharingResult
    -- the type environment calculated by HPT
    , _psTypeEnv        :: Maybe TypeEnv
    -- the type environment parsed from the source code
    , _psTypeAnnots     :: TypeEnv
    , _psEffectMap      :: Maybe EffectMap
    , _psErrors         :: [String]
    , _psIntendation    :: Int
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


-- NOTE: All the return types of the transformations should be the same.
data TransformationFunc
  = Plain          (Exp -> (Exp, ExpChanges))
  | WithTypeEnv    (TypeEnv -> Exp -> Either String (Exp, ExpChanges))
  | WithTypeEnvEff (TypeEnv -> EffectMap -> Exp -> (Exp, ExpChanges))
  | WithTypeEnvShr (Sharing.SharingResult -> TypeEnv -> Exp -> (Exp, ExpChanges))
  | WithLVA        (LVA.LVAResult -> TypeEnv -> Exp -> Either String (Exp, ExpChanges))
  | WithLVACBy     (LVA.LVAResult -> CBy.CByResult -> TypeEnv -> Exp -> Either String (Exp, ExpChanges))

-- TODO: Add n paramter for the transformations that use NameM
transformationFunc :: Int -> Transformation -> TransformationFunc
transformationFunc n = \case
  Vectorisation                   -> WithTypeEnv (newNames <$$> Right <$$> Vectorisation2.vectorisation)
  GenerateEval                    -> Plain generateEval
  CaseSimplification              -> Plain (noNewNames . caseSimplification)
  SplitFetch                      -> Plain (noNewNames . splitFetch)
  RegisterIntroduction            -> Plain (newNames . registerIntroductionI n) -- TODO
  ProducerNameIntroduction        -> Plain producerNameIntroduction
  BindingPatternSimplification    -> Plain bindingPatternSimplification
  RightHoistFetch                 -> Plain (noNewNames . RHF.rightHoistFetch)
  -- misc
  MangleNames                     -> Plain (newNames . mangleNames) -- TODO
  StaticSingleAssignment          -> Plain (newNames . staticSingleAssignment) -- TODO
  BindNormalisation               -> Plain (noNewNames . bindNormalisation)
  ConstantFolding                 -> Plain (newNames . constantFolding)
  -- optimising
  EvaluatedCaseElimination        -> Plain (noNewNames . evaluatedCaseElimination)
  TrivialCaseElimination          -> Plain (noNewNames . trivialCaseElimination)
  UpdateElimination               -> Plain (noNewNames . updateElimination)
  CopyPropagation                 -> Plain (noNewNames . copyPropagation) -- TODO
  ConstantPropagation             -> Plain (noNewNames . constantPropagation) -- TODO
  SimpleDeadFunctionElimination   -> Plain (noNewNames . simpleDeadFunctionElimination)
  SimpleDeadParameterElimination  -> Plain (noNewNames . simpleDeadParameterElimination)
  SimpleDeadVariableElimination   -> WithTypeEnvEff (noNewNames <$$$> simpleDeadVariableElimination)
  InlineEval                      -> WithTypeEnv (Right <$$> inlineEval)
  InlineApply                     -> WithTypeEnv (Right <$$> inlineApply)
  InlineBuiltins                  -> WithTypeEnv (Right <$$> inlineBuiltins)
  CommonSubExpressionElimination  -> WithTypeEnvEff (noNewNames <$$$> commonSubExpressionElimination)
  CaseCopyPropagation             -> Plain caseCopyPropagation
  CaseHoisting                    -> WithTypeEnv (Right <$$> caseHoisting)
  GeneralizedUnboxing             -> WithTypeEnv (Right <$$> generalizedUnboxing)
  ArityRaising                    -> WithTypeEnv (Right <$$> (arityRaising n))
  LateInlining                    -> WithTypeEnv (Right <$$> lateInlining)
  UnitPropagation                 -> WithTypeEnv (noNewNames <$$> Right <$$> unitPropagation)
  NonSharedElimination            -> WithTypeEnvShr nonSharedElimination
  DeadFunctionElimination         -> WithLVA (noNewNames <$$$$> deadFunctionElimination)
  DeadVariableElimination         -> WithLVA (noNewNames <$$$$> deadVariableElimination)
  DeadParameterElimination        -> WithLVA (noNewNames <$$$$> deadParameterElimination)
  DeadDataElimination             -> WithLVACBy deadDataElimination
  SparseCaseOptimisation          -> WithTypeEnv (noNewNames <$$$> sparseCaseOptimisation)
  where
    noNewNames    = flip (,) NoChange
    newNames      = flip (,) NewNames

transformation :: Transformation -> PipelineM ()
transformation t = do
  runAnalysisFor t
  n <- use psTransStep
  e <- use psExp
  te <- fromMaybe (traceShow "empty type env is used" emptyTypeEnv) <$> use psTypeEnv
  em <- fromMaybe (traceShow "empty effect map is used" mempty) <$> use psEffectMap
  et <- fromMaybe (traceShow "empty effect tracking result is used" mempty) <$> use psETResult
  cby <- fromMaybe (traceShow "empty created by result is used" CBy.emptyCByResult) <$> use psCByResult
  lva <- fromMaybe (traceShow "empty live variable result is used" LVA.emptyLVAResult) <$> use psLVAResult
  shr <- fromMaybe (traceShow "empty sharing result is used" Sharing.emptySharingResult) <$> use psSharingResult
  either (\e -> psErrors %= (e:)) onExp $
    case transformationFunc n t of
      Plain          f -> Right $ f e
      WithTypeEnv    f -> f te e
      WithTypeEnvEff f -> Right $ f te em e
      WithLVA        f -> f lva te e
      WithLVACBy     f -> f lva cby te e
      WithTypeEnvShr f -> Right $ f shr te e
  psTransStep %= (+1)
  where
    onExp (e, changes) = do
      psExp .= e
      when (changes /= NoChange) invalidateAnalysisResults

pipelineStep :: PipelineStep -> PipelineM PipelineEff
pipelineStep p = do
  inceaseIntendation
  i <- use psIntendation
  case p of
    Pass{} -> pipelineLog "Pass"
    _ | isPrintingStep p -> pipelineLog     $ printf ("PipelineStep: %-" ++ show (80 - 2 * i) ++ "s") (show p)
    _                    -> pipelineLogNoLn $ printf ("PipelineStep: %-" ++ show (80 - 2 * i) ++ "s") (show p)
  before <- use psExp
  start <- liftIO getCurrentTime
  case p of
    Optimize -> optimizeWithM [] defaultOptimizations []

    HPT step -> case step of
      Compile       -> compileAbstractProgram HPT.codeGen psHPTProgram
      Optimise      -> optimiseAbsProgWith psHPTProgram "HPT program is not available to be optimized"
      PrintProgram  -> printAbstractProgram psHPTProgram
      SaveProgram p -> saveAbstractProgram p psHPTProgram
      RunPure       -> runHPTPure
      PrintResult   -> printAnalysisResult psHPTResult

    CBy step -> case step of
      Compile       -> compileAbstractProgram CBy.codeGen psCByProgram
      Optimise      -> optimiseAbsProgWith psCByProgram "CBy program is not available to be optimized"
      PrintProgram  -> printAbstractProgram psCByProgram
      SaveProgram p -> saveAbstractProgram p psCByProgram
      RunPure       -> runCByPure
      PrintResult   -> printAnalysisResult psCByResult

    LVA step -> case step of
      Compile       -> compileAbstractProgram LVA.codeGen psLVAProgram
      Optimise      -> optimiseAbsProgWith psLVAProgram "LVA program is not available to be optimized"
      PrintProgram  -> printAbstractProgram psLVAProgram
      SaveProgram p -> saveAbstractProgram p psLVAProgram
      RunPure       -> runLVAPure
      PrintResult   -> printAnalysisResult psLVAResult

    ET step -> case step of
      Compile       -> compileAbstractProgram ET.codeGen psETProgram
      Optimise      -> optimiseAbsProgWith psETProgram "ET program is not available to be optimized"
      PrintProgram  -> printAbstractProgram psETProgram
      RunPure       -> runETPure
      PrintResult   -> printAnalysisResult psETResult

    RunCByWithLVA -> runCByWithLVAPure

    Sharing step -> case step of
      Compile       -> compileAbstractProgram Sharing.codeGen psSharingProgram
      Optimise      -> optimiseAbsProgWith psSharingProgram "Sharing program is not available to be optimized"
      PrintProgram  -> printAbstractProgram psSharingProgram
      SaveProgram p -> saveAbstractProgram p psSharingProgram
      RunPure       -> runSharingPure
      PrintResult   -> printAnalysisResult psSharingResult

    Eff eff -> case eff of
      CalcEffectMap   -> calcEffectMap
      PrintEffectMap  -> printEffectMap
    T t             -> transformation t
    Pass pass       -> mapM_ pipelineStep pass
    PrintGrin r d   -> printGrinM r d
    JITLLVM         -> jitLLVM
    SaveLLVM path   -> saveLLVM path
    SaveExecutable dbg path -> saveExecutable dbg path
    SaveGrin path   -> saveGrin path
    SaveBinary name -> saveBinary name
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
    PureEval                  showStatistics -> pureEval (EvalPlugin evalPrimOp) showStatistics
    PureEvalPlugin evalPlugin showStatistics -> pureEval evalPlugin showStatistics
    DefinitionalInterpreter evalPlugin showStatistics -> definionalInterpreterEval evalPlugin showStatistics
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
  when (eff == ExpChanged) $ psSaveIdx %= succ
  -- TODO: Test this only for development mode.
  decreateIntendation
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

optimiseAbsProgWith :: Lens' PState (Maybe (IR.AbstractProgram, a)) -> String -> PipelineM ()
optimiseAbsProgWith getProg err = do
  mProg <- use getProg
  case mProg of
    Just prog -> getProg . _Just . _1 %= optimiseAbstractProgram
    Nothing   -> pipelineLog err

compileAbstractProgram :: (Exp -> prog) -> (Lens' PState (Maybe prog)) -> PipelineM ()
compileAbstractProgram codeGen accessProg = do
  grin <- use psExp
  accessProg .= Just (codeGen grin)

printAbsProg :: IR.AbstractProgram -> PipelineM ()
printAbsProg a = do
  pipelineLog $ show $ IR.prettyInstructions Nothing . IR._absInstructions $ a
  pipelineLog $ printf "memory size    %d" $ IR._absMemoryCounter a
  pipelineLog $ printf "register count %d" $ IR._absRegisterCounter a
  -- TODO: pass mapping
  --pipelineLog $ printf "variable count %d" $ Map.size $ IR._absRegisterMap a

printAbstractProgram :: (Lens' PState (Maybe (IR.AbstractProgram, a))) -> PipelineM ()
printAbstractProgram accessProg = do
  progM <- use accessProg
  mapM_ (printAbsProg . fst) progM

saveAbstractProgram :: String -> (Lens' PState (Maybe (IR.AbstractProgram, a))) -> PipelineM ()
saveAbstractProgram name accessProg = do
  progM <- use accessProg
  n <- use psSaveIdx
  case progM of
    Nothing   -> pure ()
    Just (prog, mapping) -> do
      outputDir <- view poOutputDir
      let fname = printf "%03d.%s.dfbin" n name
      liftIO $ LBS.writeFile (outputDir </> fname) $ encodeAbstractProgram prog

printAnalysisResult :: Pretty res => (Lens' PState (Maybe res)) -> PipelineM ()
printAnalysisResult accessRes = use accessRes >>= \case
  Nothing -> pure ()
  Just result -> pipelineLog $ show $ pretty result


runHPTPure :: PipelineM ()
runHPTPure = use psHPTProgram >>= \case
  Nothing -> psHPTResult .= Nothing
  Just (hptProgram, hptMapping) -> do
    let AbsIntResult{..} = evalAbstractProgram hptProgram
        result = HPT.toHPTResult hptMapping _airComp
    pipelineLogIterations _airIter
    psHPTResult .= Just result
    case typeEnvFromHPTResult result of
      Right te  -> psTypeEnv .= Just te
      Left err  -> do
        psErrors %= (err :)
        liftIO $ printf "type-env error: %s" err
        psTypeEnv .= Nothing


runCByPureWith :: (CBy.CByMapping -> ComputerState -> CBy.CByResult) -> PipelineM ()
runCByPureWith toCByResult = use psCByProgram >>= \case
  Nothing -> psCByResult .= Nothing
  Just (cbyProgram, cbyMapping) -> do
    let AbsIntResult{..} = evalAbstractProgram cbyProgram
        result = toCByResult cbyMapping _airComp
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
  Just (lvaProgram, lvaMapping) -> do
    let AbsIntResult{..} = evalAbstractProgram $ lvaProgram
        result = LVA.toLVAResult lvaMapping _airComp
    pipelineLogIterations _airIter
    psLVAResult .= Just result

runETPure :: PipelineM ()
runETPure = use psETProgram >>= \case
  Nothing -> psETResult .= Nothing
  Just (etProgram, etMapping) -> do
    let AbsIntResult{..} = evalAbstractProgram $ etProgram
        result = ET.toETResult etMapping _airComp
    pipelineLogIterations _airIter
    psETResult .= Just result

runSharingPureWith :: (Sharing.SharingMapping -> ComputerState -> Sharing.SharingResult) -> PipelineM ()
runSharingPureWith toSharingResult = use psSharingProgram >>= \case
  Nothing -> psSharingResult .= Nothing
  Just (shrProgram, shrMapping) -> do
    let AbsIntResult{..} = evalAbstractProgram shrProgram
        result = toSharingResult shrMapping _airComp
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

pureEval :: EvalPlugin -> Bool -> PipelineM ()
pureEval evalPlugin showStatistics = do
  e <- use psExp
  (val, stat) <- liftIO $ do
    hSetBuffering stdout NoBuffering
    evalProgram (PureReducer evalPlugin) e
  when showStatistics $ pipelineLog $ show $ pretty stat
  pipelineLog $ show $ pretty val

definionalInterpreterEval :: EvalPlugin -> Bool -> PipelineM ()
definionalInterpreterEval evalPlugin showStatistics = do
  e <- use psExp
  (val, stat) <- liftIO $ do
    hSetBuffering stdout NoBuffering
    evalProgram (DefinitionalReducer evalPlugin) e
  when showStatistics $ pipelineLog $ show $ pretty stat
  pipelineLog $ show $ pretty val

printGrinM :: RenderingOption -> (Doc -> Doc) -> PipelineM ()
printGrinM r color = do
  p <- use psExp
  pipelineLog $ showWide $ color $ prettyProgram r p

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
  e <- use psExp
  case path of
    Rel fn -> saveTransformationInfo fn e
    Abs fn -> liftIO $ do
      writeFile fn $ show $ plain $ pretty e

-- | Save binary similar as transformation info.
saveBinary :: String -> PipelineM ()
saveBinary name = do
  n <- use psSaveIdx
  ent <- Nametable.convert <$> use psExp
  outputDir <- view poOutputDir
  let fname = printf "%03d.%s.binary" n name
  liftIO $ Binary.encodeFile (outputDir </> fname) ent

relPath :: Path -> PipelineM String
relPath path = do
  n <- use psSaveIdx
  o <- view poOutputDir
  pure $ case path of
    Abs fname -> fname
    Rel fname -> o </> printf "%03d.%s" n fname

callCommand :: String -> PipelineM ()
callCommand cmd = do
  pipelineLog $ "Call command:" ++ cmd
  liftIO $ System.Process.callCommand cmd

saveLLVM :: Path -> PipelineM ()
saveLLVM path = do
  e <- use psExp
  pipelineStep HPTPass
  Just typeEnv <- use psTypeEnv
  fname <- relPath path
  let code = CGLLVM.codeGen typeEnv e
  let llName = printf "%s.ll" fname
  let sName = printf "%s.s" fname
  pipelineLog "* to LLVM *"
  void $ liftIO $ CGLLVM.toLLVM llName code
  pipelineLog"* LLVM X64 codegen *"
  llcExe <- liftIO $ fromMaybe "llc-7" <$> lookupEnv "GRIN_LLC"
  optExe <- liftIO $ fromMaybe "opt-7" <$> lookupEnv "GRIN_OPT"
  callCommand $ printf "%s -O3 %s | %s -o %s" optExe llName llcExe (sName :: String)

saveExecutable :: Bool -> Path -> PipelineM ()
saveExecutable debugSymbols path = do
  pipelineLog "* generate llvm x64 optcode *"
  let grinOptCodePath = Rel "grin-opt-code"
  clangExe <- liftIO $ fromMaybe "clang-7" <$> lookupEnv "GRIN_CC"
  llcExe <- liftIO $ fromMaybe "llc-7" <$> lookupEnv "GRIN_LLC"
  pipelineStep $ SaveLLVM grinOptCodePath
  grinOptCodeFile <- relPath grinOptCodePath
  fname <- relPath path
  pipelineLog "* generate executable *"
  callCommand $ printf
    ("%s -O3 -relocation-model=pic -filetype=obj %s.ll" ++ if debugSymbols then " -debugger-tune=gdb" else "")
    llcExe grinOptCodeFile
  cfg <- ask
  callCommand $ printf
    -- TODO: Support defining libraries for ffi and primops.
    ("%s -lm -O3 %s %s.o -s -o %s" ++ if debugSymbols then " -g" else "")
    clangExe (intercalate " " $ _poCFiles cfg) grinOptCodeFile fname

debugTransformation :: (Exp -> Exp) -> PipelineM ()
debugTransformation t = do
  e <- use psExp
  liftIO . print $ pretty (t e)

lintGrin :: Maybe String -> PipelineM ()
lintGrin mPhaseName = do
  o <- ask
  when (o ^. poTypedLint) $ void $ do
    pipelineStep $ HPT Compile
    pipelineStep $ HPT RunPure
  exp <- use psExp
  mTypeEnv <- use psTypeEnv
  -- By default we don't run the DDE related warnings. They should be enabled
  -- when we do refactor on transformations to not to create non-DDE conforming
  -- nodes, and they should be removed when we refactor the possible syntax.
  let lintExp@(_, errorMap) = Lint.lint Lint.noDDEWarnings mTypeEnv exp
  psErrors .= (fmap Lint.message $ concat $ Map.elems errorMap)

  -- print errors
  errors <- use psErrors
  unless (Prelude.null errors) $ void $ do
    case mPhaseName of
      Just phaseName  -> pipelineLog $ printf "error after %s:\n%s" phaseName (unlines errors)
      Nothing         -> pipelineLog $ printf "error:\n%s" (unlines errors)
    saveTransformationInfo "Lint" $ prettyLintExp lintExp
    mHptResult <- use psHPTResult
    saveTransformationInfo "HPT-Result" mHptResult
    failOnLintError <- view poFailOnLint
    when failOnLintError $ do
      -- FIXME: reenable after: undefined support ; transformation to inject default alts for pattern match errors
      liftIO $ die "illegal code"
      pure ()

-- confluence testing

randomPipeline :: StdGen -> PipelineOpts -> Exp -> IO Exp
randomPipeline seed opts exp
  = fmap snd $ runPipeline opts emptyTypeEnv exp $ randomPipelineM seed

-- Generate random pipeline based on the transformationWhitelist, the pipeline reaches a fixpoint
-- and returns the list of transformation that helped to reach the fixpoint.
randomPipelineM :: StdGen -> PipelineM [Transformation]
randomPipelineM seed = do
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
      , ET Compile
      , ET RunPure
      ]

    runCByLVA :: PipelineM ()
    runCByLVA = mapM_ pipelineStep
      [ CBy Compile
      , CBy RunPure
      , LVA Compile
      , LVA RunPure
      , ET Compile
      , ET RunPure
      ]

    runNameIntro :: PipelineM ()
    runNameIntro = void . pipelineStep $ Pass
      [ T ProducerNameIntroduction
      , T BindNormalisation
      , T BindingPatternSimplification
      , T BindNormalisation
      ]

    -- cleanup after producer name intro
    runCleanup :: PipelineM ()
    runCleanup = void . pipelineStep $ Pass
      [ T CopyPropagation
      , T SimpleDeadVariableElimination
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
  pipeline1 <- randomPipelineM gen1
  pipelineLog "Random pipeline #2"
  exp1 <- use psExp
  MonadState.put state
  gen2 <- liftIO newStdGen
  pipeline2 <- randomPipelineM gen2
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
      , _psETProgram      = Nothing
      , _psETResult       = Nothing
      , _psSharingResult  = Nothing
      , _psSharingProgram = Nothing
      , _psTypeEnv        = Nothing
      , _psTypeAnnots     = ta
      , _psEffectMap      = Nothing
      , _psErrors         = []
      , _psIntendation    = 0
      }

-- | Runs the pipeline and returns the last version of the given
-- expression.
pipeline :: PipelineOpts -> Maybe TypeEnv -> Exp -> [PipelineStep] -> IO Exp
pipeline o mte e ps = fmap snd $ runPipeline o (fromMaybe emptyTypeEnv mte) e $ mapM pipelineStep ps

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

    phaseLoop _         [] = pure False
    phaseLoop isChanged ts = do
      o <- ask
      effs <- forM (BindNormalisation:ts) $ \t -> do
        eff <- pipelineStep (T t)
        when (eff == ExpChanged) $ do
          let tname = (fmap (\case ' ' -> '-' ; c -> c) $ show t)
          pipelineStep $ SaveGrin $ Rel $ tname <.> "grin"
          when (o ^. poSaveBinary) $ void $ pipelineStep $ SaveBinary tname
          when (o ^. poLintOnChange) $ lintGrin $ Just $ show t
          when (o ^. poStatistics)  $ void $ pipelineStep Statistics
          when (o ^. poSaveTypeEnv) $ void $ pipelineStep SaveTypeEnv
        pure eff
      if (any (==ExpChanged) effs)
        then phaseLoop True ts
        else pure isChanged

    -- No analysis is required
    phase1 = phaseLoop False $ trans `intersect`
      [ EvaluatedCaseElimination
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
      , CommonSubExpressionElimination
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
          [ map T
              [ CopyPropagation
              , SimpleDeadVariableElimination
              , ProducerNameIntroduction
              , BindNormalisation
              , BindingPatternSimplification
              , BindNormalisation
              , UnitPropagation
              ]
          , map T $ trans `intersect`
              [ DeadFunctionElimination
              , DeadDataElimination
              , DeadVariableElimination
              , DeadParameterElimination
              ]
          , map T
              [ CopyPropagation
              , SimpleDeadVariableElimination
              , BindNormalisation
              , UnitPropagation
              ]
          ]

invalidateAnalysisResults :: PipelineM ()
invalidateAnalysisResults = do
  pipelineLog "Invalidating type environment"
  psHPTProgram     .= Nothing
  psHPTResult      .= Nothing
  psCByProgram     .= Nothing
  psCByResult      .= Nothing
  psLVAProgram     .= Nothing
  psLVAResult      .= Nothing
  psETProgram      .= Nothing
  psETResult       .= Nothing
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
    WithLVACBy     _ -> [hpt,     cby, lva, sharing]
    WithTypeEnvShr _ -> [hpt, sharing]
  where
    analysis getter ann = do
      r <- use getter
      when (isNothing r) $ do
        pipelineLog ""
        pipelineLog $ "Analysis"
        mapM_ pipelineStep $ (ann <$> [Compile, RunPure])

    hpt = analysis psHPTResult HPT
    lva = analysis psLVAResult LVA
    cby = analysis psCByResult CBy
    et  = analysis psETResult ET
    sharing = analysis psSharingResult Sharing

    eff :: PipelineM ()
    eff = do
      r <- use psEffectMap
      when (isNothing r) $ do
        pipelineLog ""
        pipelineLog $ "Analysis"
        void $ pipelineStep $ Eff CalcEffectMap

inceaseIntendation :: PipelineM ()
inceaseIntendation = psIntendation %= succ

decreateIntendation :: PipelineM ()
decreateIntendation = psIntendation %= pred

pipelineLog :: String -> PipelineM ()
pipelineLog str = do
  shouldLog <- view poLogging
  ident <- use psIntendation
  when shouldLog $ liftIO $ putStrLn $ replicate ident ' ' ++ str

pipelineLogNoLn :: String -> PipelineM ()
pipelineLogNoLn str = do
  shouldLog <- view poLogging
  ident <- use psIntendation
  when shouldLog $ liftIO $ putStr $ replicate ident ' ' ++ str

pipelineLogIterations :: Int -> PipelineM ()
pipelineLogIterations n = pipelineLogNoLn $ "iterations: " ++ show n ++ " "

defaultOptimizations :: [Transformation]
defaultOptimizations =
  [ InlineEval
  , SparseCaseOptimisation
  , SimpleDeadFunctionElimination
  , SimpleDeadParameterElimination
  , SimpleDeadVariableElimination
  , EvaluatedCaseElimination
  , TrivialCaseElimination
  , UpdateElimination
  , NonSharedElimination
  , CopyPropagation
  , ConstantPropagation
  , CommonSubExpressionElimination
  , CaseCopyPropagation
  , CaseHoisting
  , GeneralizedUnboxing
  , ArityRaising
  , InlineApply
  , LateInlining
  ]

debugPipeline :: [PipelineStep] -> [PipelineStep]
debugPipeline ps = [SimplePrintGrin id] ++ ps ++ [SimplePrintGrin id]

debugPipelineState :: PipelineM ()
debugPipelineState = do
  ps <- MonadState.get
  liftIO $ print ps

printingSteps :: [PipelineStep]
printingSteps =
  [ HPT PrintProgram
  , HPT PrintResult
  , CBy PrintProgram
  , CBy PrintResult
  , LVA PrintProgram
  , LVA PrintResult
  , ET  PrintProgram
  , ET  PrintResult
  , Sharing PrintProgram
  , Sharing PrintResult
  , PrintTypeEnv
  , Eff PrintEffectMap
  , PrintAST
  , PrintErrors
  , PrintTypeAnnots
  , DebugPipelineState
  , SimplePrintGrin id
  ]

isPrintingStep :: PipelineStep -> Bool
isPrintingStep = flip elem printingSteps
