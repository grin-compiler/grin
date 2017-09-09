# Todo ideas

- distinguish Val constuctors (LambdaPattern ConstansPattern SimpleValue Value)
- rename Val types to a more descriptive names; see above
- GRIN AST gen EDSL
- opt-parse-applicative for command line arguments for cli (grin)
- better name representation instead of strings
- generate unique names when branching (unique name A + branching direction = unique name B)
- efficient substitution
- bind normalisation
- type safer and easy to use AST
- add fetch item / indexed fetch (choose a good name)
- add simple frontend language with grin conversion
- generate apply

# JIT requirements

simplifaction transformations required by the codegen

- [ ] vectorisation
- [x] case simplification
- [x] split fetch operation
- [ ] right hoist fetch operation
- [x] register introduction

