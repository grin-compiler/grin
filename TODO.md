# Todo ideas

- distinguish Val constuctors (LambdaPattern ConstansPattern SimpleValue Value)
- rename Val types to a more descriptive names; see above
- GRIN AST gen EDSL
- better name representation instead of strings
- generate unique names when branching (unique name A + branching direction = unique name B)
- efficient substitution
- type safer and easy to use AST
- add simple frontend language with grin conversion (GHC/STG -> GRIN)
- generate apply


# CodeGen

- simplify/refactor LLVM codegen
- vectorisation = tagged union conversion (on branch: var-tag-node-is-tagged-union) ; better syntax for tagged union (parser + pretty printer)
- read back LLVM reduced result to Haskell data (including the heap)

# GRIN framework

- scoped type environment
- scoped names ; it would make inlining easy (the idea is similar to bound)
  - HPT
  - optimisiations
  - type env

# HPT

- LLVM backend for HPT IR
- inline support
- sharing analysis
