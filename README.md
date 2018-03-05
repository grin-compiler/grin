# GRIN

[![Build Status](https://travis-ci.org/andorp/grin.svg?branch=master)](https://travis-ci.org/andorp/grin)

The name GRIN is short for *Graph Reduction Intermediate Notation*, and it is an intermediate language for graph reduction.
For an overview read
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/The GRIN Project.pdf">
The GRIN Project
</a> article. To grasp the details take your time and read Urban Boquist's PhD thesis on
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf">
Code Optimisation Techniques for Lazy Functional Languages
</a>.

<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=41">
<img src="https://raw.githubusercontent.com/andorp/grin/master/images/grin-syntax.png" width="500" >
</a>

## Simplifying Transformations

Transformation | Schema | Source Code
-------------- | ------ | -----------
[vectorisation][113]                | [<img src="images/vectorisation.png"         width="500">][113] | [Vectorisation2.hs]
[case simplification][116]          | [<img src="images/case-simplification.png"   width="500">][116] | [CaseSimplification.hs]
[split fetch operation][118]        | [<img src="images/split-fetch-operation.png" width="500">][118] | [SplitFetch.hs]
[right hoist fetch operation][123]  | [<img src="images/right-hoist-fetch.png"     width="500">][123] | [RightHoistFetch2.hs]
[register introduction][126]        | [<img src="images/register-introduction.png" width="500">][126] | [RegisterIntroduction.hs]


[113]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=113
[116]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=116
[118]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=118
[123]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=123
[126]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=126

[Vectorisation2.hs]:        https://github.com/andorp/grin/blob/master/grin/src/Transformations/Simplifying/Vectorisation2.hs
[CaseSimplification.hs]:    https://github.com/andorp/grin/blob/master/grin/src/Transformations/Simplifying/CaseSimplification.hs
[SplitFetch.hs]:            https://github.com/andorp/grin/blob/master/grin/src/Transformations/Simplifying/SplitFetch.hs
[RightHoistFetch2.hs]:      https://github.com/andorp/grin/blob/master/grin/src/Transformations/Simplifying/RightHoistFetch2.hs
[RegisterIntroduction.hs]:  https://github.com/andorp/grin/blob/master/grin/src/Transformations/Simplifying/RegisterIntroduction.hs


## Optimising Transformations

Transformation | Schema | Source Code
-------------- | ------ | -----------
[evaluated case elimination][141]         | [<img src="images/evaluated-case-elimination.png" width="500">][141]  | [EvaluatedCaseElimination.hs]
[trivial case elimination][142]           | [<img src="images/trivial-case-elimination.png"   width="500">][142]  | [TrivialCaseElimination.hs]
[sparse case optimisation][143]           | [<img src="images/sparse-case-optimisation.png"   width="500">][143]  | [SparseCaseOptimisation.hs]
[update elimination][148]                 | [<img src="images/update-elimination.png"         width="500">][148]  | [UpdateElimination.hs]
[copy propagation][129]                   | [<img src="images/copy-propagation-left.png"      width="500"><img src="images/copy-propagation-right.png" width="500">][129]  | [CopyPropagation.hs]
[late inlining][151]                      | [<img src="images/late-inlining.png"              width="500">][151]  | [Inlining.hs]
[generalised unboxing][134]               | [<img src="images/generalised-unboxing.png"       width="500"><img src="images/unboxing-of-function-return-values.png" width="500">][134]  | TODO
[arity raising][160]                      | [<img src="images/arity-raising.png"              width="500">][160]  | TODO
[case copy propagation][144]              | [<img src="images/case-copy-propagation.png"      width="500">][144]  | TODO
[case hoisting][153]                      | [<img src="images/case-hoisting.png"              width="500">][153]  | TODO
[whnf update elimination][149]            | [<img src="images/whnf-update-elimination.png"    width="500">][149]  | TODO
[common sub-expression elimination][164]  | [<img src="images/common-sub-expression-elimination-1.png" width="500"><img src="images/common-sub-expression-elimination-2.png" width="500">][164]  | [CSE.hs]
[constant propagation][159]               |   | [ConstantPropagation.hs]
[dead procedure elimination][169]         |   | [DeadProcedureElimination.hs]
[dead variable elimination][170]          |   | [DeadVariableElimination.hs]
[dead parameter elimination][171]         |   | TODO

[129]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=129
[134]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=134
[141]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=141
[142]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=142
[143]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=143
[144]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=144
[148]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=148
[149]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=149
[151]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=151
[153]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=153
[159]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=159
[160]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=160
[164]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=164
[169]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=169
[170]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=170
[171]: http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=171

[ConstantPropagation.hs]:       https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/ConstantPropagation.hs
[CopyPropagation.hs]:           https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/CopyPropagation.hs
[CSE.hs]:                       https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/CSE.hs
[DeadProcedureElimination.hs]:  https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/DeadProcedureElimination.hs
[DeadVariableElimination.hs]:   https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/DeadVariableElimination.hs
[EvaluatedCaseElimination.hs]:  https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/EvaluatedCaseElimination.hs
[Inlining.hs]:                  https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/Inlining.hs
[SparseCaseOptimisation.hs]:    https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/SparseCaseOptimisation.hs
[TrivialCaseElimination.hs]:    https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/TrivialCaseElimination.hs
[UpdateElimination.hs]:         https://github.com/andorp/grin/blob/master/grin/src/Transformations/Optimising/UpdateElimination.hs
