# builtin types

- word
- int
- float
- string (c string)

# primops

- add / sub (float + int + word)
- mul / div (float + int + word)
- gt / ge / lt / le (float + int + word)
- eq / ne (float + int + word + string)
- print / readLine (string)

# features
- node support for codegen
- support for builtin types and primops
- single module (no module system)

# documentation
- HPT
- llvm codegen ; HPT as LLVM type inference
- shape functor ; pro / cons

# sample programs
- observe compilation stages: GRIN -> LLVM -> x64
