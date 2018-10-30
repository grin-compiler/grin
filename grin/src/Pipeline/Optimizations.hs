module Pipeline.Optimizations
  ( constantFolding
  , evaluatedCaseElimination
  , trivialCaseElimination
  , sparseCaseOptimisation
  , updateElimination
  , copyPropagation
  , constantPropagation
  , deadDataElimination
  , deadParameterElimination
  , simpleDeadFunctionElimination
  , simpleDeadVariableElimination
  , simpleDeadParameterElimination
  , commonSubExpressionElimination
  , caseCopyPropagation
  , generalizedUnboxing
  , arityRaising
  , caseHoisting
  , lateInlining
  ) where

import Transformations.Optimising.ConstantFolding (constantFolding)
import Transformations.Optimising.EvaluatedCaseElimination (evaluatedCaseElimination)
import Transformations.Optimising.TrivialCaseElimination (trivialCaseElimination)
import Transformations.Optimising.SparseCaseOptimisation (sparseCaseOptimisation)
import Transformations.Optimising.UpdateElimination (updateElimination)
import Transformations.Optimising.CopyPropagation (copyPropagation)
import Transformations.Optimising.ConstantPropagation (constantPropagation)
import Transformations.Optimising.DeadDataElimination (deadDataElimination)
import Transformations.Optimising.DeadParameterElimination (deadParameterElimination)
import Transformations.Optimising.SimpleDeadFunctionElimination (simpleDeadFunctionElimination)
import Transformations.Optimising.SimpleDeadVariableElimination (simpleDeadVariableElimination)
import Transformations.Optimising.SimpleDeadParameterElimination (simpleDeadParameterElimination)
import Transformations.Optimising.CSE (commonSubExpressionElimination)
import Transformations.Optimising.CaseCopyPropagation (caseCopyPropagation)
import Transformations.Optimising.GeneralizedUnboxing (generalizedUnboxing)
import Transformations.Optimising.ArityRaising (arityRaising)
import Transformations.Optimising.CaseHoisting (caseHoisting)
import Transformations.Optimising.Inlining (lateInlining)
