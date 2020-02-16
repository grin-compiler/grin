module Pipeline.ExtendedSyntax.Optimizations
  ( evaluatedCaseElimination
  , trivialCaseElimination
  , sparseCaseOptimisation
  , copyPropagation
  , constantPropagation
  , interproceduralDeadDataElimination
  , interproceduralDeadFunctionElimination
  , interproceduralDeadParameterElimination
  , deadFunctionElimination
  , deadVariableElimination
  , deadParameterElimination
  , commonSubExpressionElimination
  , caseCopyPropagation
  , generalizedUnboxing
  , arityRaising
  , caseHoisting
  , lateInlining
  , nonSharedElimination
  ) where

import Transformations.ExtendedSyntax.Optimising.EvaluatedCaseElimination (evaluatedCaseElimination)
import Transformations.ExtendedSyntax.Optimising.TrivialCaseElimination (trivialCaseElimination)
import Transformations.ExtendedSyntax.Optimising.SparseCaseOptimisation (sparseCaseOptimisation)
import Transformations.ExtendedSyntax.Optimising.CopyPropagation (copyPropagation)
import Transformations.ExtendedSyntax.Optimising.ConstantPropagation (constantPropagation)
import Transformations.ExtendedSyntax.Optimising.InterproceduralDeadDataElimination (interproceduralDeadDataElimination)
import Transformations.ExtendedSyntax.Optimising.InterproceduralDeadFunctionElimination (interproceduralDeadFunctionElimination)
import Transformations.ExtendedSyntax.Optimising.InterproceduralDeadParameterElimination (interproceduralDeadParameterElimination)
import Transformations.ExtendedSyntax.Optimising.DeadFunctionElimination (deadFunctionElimination)
import Transformations.ExtendedSyntax.Optimising.DeadVariableElimination (deadVariableElimination)
import Transformations.ExtendedSyntax.Optimising.DeadParameterElimination (deadParameterElimination)
import Transformations.ExtendedSyntax.Optimising.CSE (commonSubExpressionElimination)
import Transformations.ExtendedSyntax.Optimising.CaseCopyPropagation (caseCopyPropagation)
import Transformations.ExtendedSyntax.Optimising.GeneralizedUnboxing (generalizedUnboxing)
import Transformations.ExtendedSyntax.Optimising.ArityRaising (arityRaising)
import Transformations.ExtendedSyntax.Optimising.CaseHoisting (caseHoisting)
import Transformations.ExtendedSyntax.Optimising.Inlining (lateInlining)
import Transformations.ExtendedSyntax.Optimising.NonSharedElimination (nonSharedElimination)
