module Pipeline.Utils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class

import Lens.Micro.Mtl

import Pipeline.Definitions

import Grin.Grin
import Grin.EffectMap
import Grin.TypeEnvDefs
import AbstractInterpretation.CreatedBy.Result
import AbstractInterpretation.LiveVariable.Result
import AbstractInterpretation.Sharing.Result

pipelineLog :: String -> PipelineM ()
pipelineLog str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStrLn str

pipelineLogNoLn :: String -> PipelineM ()
pipelineLogNoLn str = do
  shouldLog <- view poLogging
  when shouldLog $ liftIO $ putStr str

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
debugPipeline ps = [PrintGrin id] ++ ps ++ [PrintGrin id]

debugPipelineState :: PipelineM ()
debugPipelineState = do
  ps <- get
  liftIO $ print ps

printingSteps :: [PipelineStep]
printingSteps =
  [ HPT PrintProgram
  , HPT PrintResult
  , CBy PrintProgram
  , CBy PrintResult
  , LVA PrintProgram
  , LVA PrintResult
  , Sharing PrintProgram
  , Sharing PrintResult
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
