{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}
module Pipeline.Definitions where

import System.FilePath

import Data.Text (Text)

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import Lens.Micro.Platform
import Lens.Micro.Internal

import Text.PrettyPrint.ANSI.Leijen (Doc)

import Grin.Grin
import Grin.EffectMap
import Grin.TypeEnvDefs
import AbstractInterpretation.Sharing
import AbstractInterpretation.HeapPointsTo
import AbstractInterpretation.CreatedBy    
import AbstractInterpretation.LiveVariable
import AbstractInterpretation.HPTResult
import AbstractInterpretation.CByResultTypes    
import AbstractInterpretation.LVAResultTypes

data Transformation
  -- Simplifying
  = RegisterIntroduction
  | ProducerNameIntroduction
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
  | DeadCodeElimination
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
  = CompileToAbstractProgram
  | OptimiseAbstractProgram
  | PrintAbstractProgram
  | RunAbstractProgramPure
  | PrintAbstractResult
  deriving (Eq, Show)

data SharingAnalysisStep
  = ComputeSharing 
  | PrintSharingResult
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
  | Sharing SharingAnalysisStep
  | RunCByWithLVA
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
  | ParseTypeAnnots
  | PrintTypeAnnots
  | PrintTypeEnv
  | SaveTypeEnv
  | Lint
  | ConfluenceTest
  | PrintErrors
  | DebugPipelineState
  deriving (Eq, Show)

pattern PrintGrin :: (Doc -> Doc) -> PipelineStep
pattern PrintGrin c <- PrintGrinH (H c)
  where PrintGrin c =  PrintGrinH (H c)

pattern DebugTransformation :: (Exp -> Exp) -> PipelineStep
pattern DebugTransformation t <- DebugTransformationH (H t)
  where DebugTransformation t =  DebugTransformationH (H t)

data PipelineOpts = PipelineOpts
  { _poOutputDir   :: FilePath
  , _poFailOnLint  :: Bool
  , _poLogging     :: Bool
  , _poSaveTypeEnv :: Bool
  , _poStatistics  :: Bool
  }

defaultOpts :: PipelineOpts
defaultOpts = PipelineOpts
  { _poOutputDir   = ".grin-output"
  , _poFailOnLint  = True
  , _poLogging     = True
  , _poSaveTypeEnv = False
  , _poStatistics  = False
  }

type PipelineM a = ReaderT PipelineOpts (StateT PState IO) a
data PState = PState
    { _psSrc           :: Maybe Text
    , _psExp           :: Exp
    , _psTransStep     :: Int
    , _psSaveIdx       :: Int
    , _psHPTProgram    :: Maybe HPTProgram
    , _psHPTResult     :: Maybe HPTResult
    , _psCByProgram    :: Maybe CByProgram
    , _psCByResult     :: Maybe CByResult
    , _psLVAProgram    :: Maybe LVAProgram
    , _psLVAResult     :: Maybe LVAResult
    , _psSharingResult :: Maybe SharingResult
    , _psTypeEnv       :: Maybe TypeEnv
    , _psTypeAnnots    :: Maybe TypeEnv
    , _psEffectMap     :: Maybe EffectMap
    , _psErrors        :: [String]
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