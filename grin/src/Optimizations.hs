module Optimizations
  ( constantFolding
  , evaluatedCaseElimination
  , trivialCaseElimination
  , sparseCaseOptimisation
  , updateElimination
  , copyPropagation
  , constantPropagation
  , deadProcedureElimination
  , deadParameterElimination
  , deadVariableElimination
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
import Transformations.Optimising.DeadProcedureElimination (deadProcedureElimination)
import Transformations.Optimising.DeadParameterElimination (deadParameterElimination)
import Transformations.Optimising.DeadVariableElimination (deadVariableElimination)
import Transformations.Optimising.CSE (commonSubExpressionElimination)
import Transformations.Optimising.CaseCopyPropagation (caseCopyPropagation)
import Transformations.Optimising.GeneralizedUnboxing (generalizedUnboxing)
import Transformations.Optimising.ArityRaising (arityRaising)
import Transformations.Optimising.CaseHoisting (caseHoisting)
import Transformations.Optimising.Inlining (lateInlining)
