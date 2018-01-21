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

## Simplification Transformations

Transformation | Schema
-------------- | ------
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=113">vectorisation</a> | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=113"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/vectorisation.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=116">case simplification</a> | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=116"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/case-simplification.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=118">split fetch operation</a> | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=118"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/split-fetch-operation.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=123">right hoist fetch operation</a> | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=123"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/right-hoist-fetch.png" width="500" ></a>
<a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=126">register introduction</a> | <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=126"><img src="https://raw.githubusercontent.com/andorp/grin/master/images/register-introduction.png" width="500" ></a>
