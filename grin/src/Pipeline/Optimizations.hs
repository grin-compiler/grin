module Pipeline.Optimizations
  ( constantFolding
  , evaluatedCaseElimination
  , trivialCaseElimination
  , sparseCaseOptimisation
  , updateElimination
  , copyPropagation
  , constantPropagation
  , deadDataElimination
  , deadFunctionElimination
  , deadParameterElimination
  , deadVariableElimination
  , simpleDeadFunctionElimination
  , simpleDeadVariableElimination
  , simpleDeadParameterElimination
  , commonSubExpressionElimination
  , caseCopyPropagation
  , generalizedUnboxing
  , arityRaising
  , caseHoisting
  , lateInlining
  , nonSharedElimination
  ) where

import Transformations.Optimising.ConstantFolding (constantFolding)
import Transformations.Optimising.EvaluatedCaseElimination (evaluatedCaseElimination)
import Transformations.Optimising.TrivialCaseElimination (trivialCaseElimination)
import Transformations.Optimising.SparseCaseOptimisation (sparseCaseOptimisation)
import Transformations.Optimising.UpdateElimination (updateElimination)
import Transformations.Optimising.CopyPropagation (copyPropagation)
import Transformations.Optimising.ConstantPropagation (constantPropagation)
import Transformations.Optimising.DeadDataElimination (deadDataElimination)
import Transformations.Optimising.DeadFunctionElimination (deadFunctionElimination)
import Transformations.Optimising.DeadParameterElimination (deadParameterElimination)
import Transformations.Optimising.DeadVariableElimination (deadVariableElimination)
import Transformations.Optimising.SimpleDeadFunctionElimination (simpleDeadFunctionElimination)
import Transformations.Optimising.SimpleDeadVariableElimination (simpleDeadVariableElimination)
import Transformations.Optimising.SimpleDeadParameterElimination (simpleDeadParameterElimination)
import Transformations.Optimising.CSE (commonSubExpressionElimination)
import Transformations.Optimising.CaseCopyPropagation (caseCopyPropagation)
import Transformations.Optimising.GeneralizedUnboxing (generalizedUnboxing)
import Transformations.Optimising.ArityRaising (arityRaising)
import Transformations.Optimising.CaseHoisting (caseHoisting)
import Transformations.Optimising.Inlining (lateInlining)
import Transformations.Optimising.NonSharedElimination (nonSharedElimination)
