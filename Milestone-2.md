# Working framework

name handling is the biggest blocker ; module support is also related

## goal: robust and working optimizer framework

duration: 2 weeks

deadline: May 6

### new additions
- module system
- name handling
  - locally new names (in block + pass to flatten out and maintain uniqueness)
  - name scopes (module, function)
  - liberal name support (like in llvm, i.e. %"any characer 1234 {}!@#$%} -"
- context aware logging framework (for errors and debugging)
- pass manager ; run passes until the fixpoint is reached
- add `allocate` memory operation to grin ; required by circular data structures
- grin syntax for type signatures
- grin syntax to declare primops with their type signature
- grin validator pass
- hpt result based dead code elimination pass

### finish
- lambda frontend
- type system
- simplification transformations (answer if vectorisation equals with a mapping to tagged unions)
- missing optimisations
  - case hoisting
  - whnf update elimination
  - dead parameter elimination
- simplify llvm codegen (factor out complex operations as passes)
