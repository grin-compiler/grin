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

Transformation | Schema
-------------- | ------
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=113">vectorisation</a>                | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=113"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/vectorisation.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=116">case simplification</a>          | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=116"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/case-simplification.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=118">split fetch operation</a>        | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=118"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/split-fetch-operation.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=123">right hoist fetch operation</a>  | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=123"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/right-hoist-fetch.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=126">register introduction</a>        | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=126"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/register-introduction.png" width="500" ></a>

## Optimising Transformations

Transformation | Schema
-------------- | ------
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=141">evaluated case elimination</a>         | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=141"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/evaluated-case-elimination.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=142">trivial case elimination</a>           | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=142"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/trivial-case-elimination.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=143">sparse case optimisation</a>           | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=143"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/sparse-case-optimisation.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=148">update elimination</a>                 | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=148"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/update-elimination.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=129">copy propagation</a>                   | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=129"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/copy-propagation-left.png" width="500" ><img src="https://raw.githubusercontent.com/andorp/grin/master/images/copy-propagation-right.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=151">late inlining</a>                      | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=151"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/late-inlining.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=134">generalised unboxing</a>               | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=134"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/generalised-unboxing.png" width="500" ><img src="https://raw.githubusercontent.com/andorp/grin/master/images/unboxing-of-function-return-values.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=160">arity raising</a>                      | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=160"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/arity-raising.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=144">case copy propagation</a>              | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=144"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/case-copy-propagation.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=153">case hoisting</a>                      | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=153"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/case-hoisting.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=149">whnf update elimination</a>            | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=149"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/whnf-update-elimination.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=164">common sub-expression elimination</a>  | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=164"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/common-sub-expression-elimination-1.png" width="500" ><img src="https://raw.githubusercontent.com/andorp/grin/master/images/common-sub-expression-elimination-2.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=159">constant propagation</a>               | 
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=169">dead procedure elimination</a>         | 
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=170">dead variable elimination</a>          | 
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=171">dead parameter elimination</a>         | 
