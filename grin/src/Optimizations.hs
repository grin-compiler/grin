module Optimizations
  ( constantFolding
  , evaluatedCaseElimination
  , trivialCaseElimination
  , sparseCaseElimination
  ) where

import Transformations.Optimising.ConstantFolding (constantFolding)
import Transformations.Optimising.EvaluatedCaseElimination (evaluatedCaseElimination)
import Transformations.Optimising.TrivialCaseElimination (trivialCaseElimination)
import Transformations.Optimising.SparseCaseElimination (sparseCaseElimination)
