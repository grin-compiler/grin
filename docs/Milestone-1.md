# MVP

## builtin types

- word
- int
- float
- bool

## primops

- add / sub (float + int + word)
- mul / div (float + int + word)
- gt / ge / lt / le (float + int + word)
- eq / ne (float + int + word + bool)

## features
- [x] node support for codegen
- [x] no garbage collector
- [x] support for builtin types and primops
- [x] single module (no module system)
- [x] GRIN type system
- [x] typed transformations
- [x] compiled HPT

## documentation
- HPT
- llvm codegen ; HPT as LLVM type inference
- shape functor ; pro / cons
- GRIN type system

## sample programs
- observe compilation stages: GRIN -> LLVM -> x64

## after the MVP
- LLVM codegen from high level GRIN without analysis
- migrate runtime values from universal representation to the optimized one
