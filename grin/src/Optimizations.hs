module Optimizations
  ( constantFolding
  , evaluatedCaseElimination
  , trivialCaseElimination
  , sparseCaseElimination
  , updateElimination
  , copyPropagation
  , constantPropagation
  , deadProcedureElimination
  , deadVariableElimination
  , commonSubExpressionElimination
  ) where

import Transformations.Optimising.ConstantFolding (constantFolding)
import Transformations.Optimising.EvaluatedCaseElimination (evaluatedCaseElimination)
import Transformations.Optimising.TrivialCaseElimination (trivialCaseElimination)
import Transformations.Optimising.SparseCaseElimination (sparseCaseElimination)
import Transformations.Optimising.UpdateElimination (updateElimination)
import Transformations.Optimising.CopyPropagation (copyPropagation)
import Transformations.Optimising.ConstantPropagation (constantPropagation)
import Transformations.Optimising.DeadProcedureElimination (deadProcedureElimination)
import Transformations.Optimising.DeadVariableElimination (deadVariableElimination)
import Transformations.Optimising.CSE (commonSubExpressionElimination)
