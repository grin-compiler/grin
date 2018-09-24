# Compiler goals

- keep mapping between source code and machine code
- debugging
  - step by step execution
  - breakpoints
  - observe values/thunks/nodes at runtime
- collect statistics at runtime
- nice UI (reusing exisiting tools; js libs, etc)

# Other

- parallel compilation
- local and global optimizer

# Technology

- simple AST + shape functor + recursion-schemes
- simple name handling
- JIT backend as a library (e.g. monad transformer?)
