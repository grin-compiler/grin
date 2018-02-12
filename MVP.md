# builtin types

- word
- int
- float
- bool

# primops

- add / sub (float + int + word)
- mul / div (float + int + word)
- gt / ge / lt / le (float + int + word)
- eq / ne (float + int + word + bool)

# features
- node support for codegen
- no garbage collector
- support for builtin types and primops
- single module (no module system)
- compiled HPT

# documentation
- HPT
- llvm codegen ; HPT as LLVM type inference
- shape functor ; pro / cons

# sample programs
- observe compilation stages: GRIN -> LLVM -> x64

# after the MVP
- LLVM codegen from high level GRIN without analysis
- migrate runtime values from universal representation to the optimized one
