# Todo ideas

- distinguish Val constuctors (LambdaPattern ConstansPattern SimpleValue Value)
- rename Val types to a more descriptive names; see above
- GRIN AST gen EDSL
- better name representation instead of strings
- generate unique names when branching (unique name A + branching direction = unique name B)
- efficient substitution
- type safer and easy to use AST
- add simple frontend language with grin conversion
- generate apply

# JIT requirements

simplifaction transformations required by the codegen

- [x] <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=113">vectorisation</a>
- [x] <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=116">case simplification</a>
- [x] <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=118">split fetch operation</a>
- [x] <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=123">right hoist fetch operation</a>
- [x] <a href="http://nbviewer.jupyter.org/github/andorp/grin/blob/master/papers/boquist.pdf#page=126">register introduction</a>

