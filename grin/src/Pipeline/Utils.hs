module Pipeline.Utils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class

import Lens.Micro.Mtl

import Pipeline.Definitions

import Grin.Grin
import Grin.EffectMap
import Grin.TypeEnvDefs
import AbstractInterpretation.CByResultTypes
import AbstractInterpretation.LVAResultTypes
import AbstractInterpretation.SharingResult

pipelineLog :: String -> PipelineM ()
pipelineLog str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStrLn str

pipelineLogNoLn :: String -> PipelineM ()
pipelineLogNoLn str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStr str

pipelineLogIterations :: Int -> PipelineM ()
pipelineLogIterations n = pipelineLogNoLn $ "iterations: " ++ show n


-- TODO: Refactor these into some kind of Maybe monad
-- TODO: This needs some serious refactoring ...

withPState :: (PState -> Maybe a) -> String -> (a -> PipelineM ()) -> PipelineM ()
withPState selector err action = do
  substateM <- gets selector
  maybe (pipelineLog err) action substateM

notAvailableMsg :: String -> String
notAvailableMsg str = str ++ " in not available, skipping next step"

withTypeEnv :: (TypeEnv -> PipelineM ()) -> PipelineM ()
withTypeEnv = withPState _psTypeEnv $ notAvailableMsg "Type environment"

withCByResult :: (CByResult -> PipelineM ()) -> PipelineM ()
withCByResult = withPState _psCByResult $ notAvailableMsg "Created-by analysis result"

withLVAResult :: (LVAResult -> PipelineM ()) -> PipelineM ()
withLVAResult = withPState _psLVAResult $ notAvailableMsg "Live variable analysis result"

withSharing :: (SharingResult -> PipelineM ()) -> PipelineM ()
withSharing = withPState _psSharingResult $ notAvailableMsg "Sharing analysis result"

withEffectMap :: (EffectMap -> PipelineM ()) -> PipelineM ()
withEffectMap = withPState _psEffectMap $ notAvailableMsg "Effect map"

withTyEnvCByLVA ::
  (TypeEnv -> CByResult -> LVAResult -> PipelineM ()) ->
  PipelineM ()
withTyEnvCByLVA f =
  withTypeEnv $ \te ->
    withCByResult $ \cby ->
      withLVAResult $ \lva ->
        f te cby lva

withEffMapTyEnvCByLVA :: 
  (EffectMap -> TypeEnv -> CByResult -> LVAResult -> PipelineM ()) ->
  PipelineM ()
withEffMapTyEnvCByLVA f = withEffectMap (withTyEnvCByLVA . f)

withTyEnvLVA ::
  (TypeEnv -> LVAResult -> PipelineM ()) ->
  PipelineM ()
withTyEnvLVA f =
  withTypeEnv $ \te ->
    withLVAResult $ \lva ->
      f te lva

withEffMapTyEnvLVA :: 
  (EffectMap -> TypeEnv -> LVAResult -> PipelineM ()) ->
  PipelineM ()
withEffMapTyEnvLVA f = withEffectMap (withTyEnvLVA . f)

withTyEnvSharing ::
  (TypeEnv -> SharingResult -> PipelineM ()) ->
  PipelineM ()
withTyEnvSharing f =
  withTypeEnv $ \te ->
    withSharing $ \shLocs ->
      f te shLocs

defaultOptimizations :: [Transformation]
defaultOptimizations =
  [ EvaluatedCaseElimination
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
  , InlineEval
  , InlineApply
  , LateInlining
  ]

defaultOnChange :: [PipelineStep]
defaultOnChange =
  [ T ProducerNameIntroduction
  , T BindNormalisation
  , CBy CompileToAbstractProgram
  , CBy RunAbstractProgramPure
  , LVA CompileToAbstractProgram
  , LVA RunAbstractProgramPure
  , Sharing CompileToAbstractProgram
  , Sharing RunAbstractProgramPure
  , T UnitPropagation
  , Eff CalcEffectMap
  ]

-- Copy propagation, SDVE and bind normalisitaion
-- together can clean up all unnecessary artifacts
-- of producer name introduction.
defaultCleanUp :: [PipelineStep]
defaultCleanUp =
  [ T CopyPropagation
  , T SimpleDeadVariableElimination
  ]

debugPipeline :: [PipelineStep] -> [PipelineStep]
debugPipeline ps = [PrintGrin id] ++ ps ++ [PrintGrin id]

debugPipelineState :: PipelineM ()
debugPipelineState = do
  ps <- get
  liftIO $ print ps

printingSteps :: [PipelineStep]
printingSteps =
  [ HPT PrintAbstractProgram
  , HPT PrintAbstractResult
  , CBy PrintAbstractProgram
  , CBy PrintAbstractResult
  , LVA PrintAbstractProgram
  , LVA PrintAbstractResult
  , Sharing PrintAbstractProgram
  , Sharing PrintAbstractResult
  , PrintTypeEnv
  , Eff PrintEffectMap
  , PrintAST
  , PrintErrors
  , PrintTypeAnnots
  , DebugPipelineState
  , PrintGrin id
  ]

isPrintingStep :: PipelineStep -> Bool
isPrintingStep = flip elem printingSteps