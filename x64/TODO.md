
# Instructions DSL

The main problem is typing the instructions in Haskell.

Unsolved problems:

-   How to support instructions with variable arguments (like imul)?
-   How to support instructions with mixed sized arguments (like bt)?
-   How to prevent more misuse statically?


# Support more instructions

-   pusha / popa ?

# Possible renaming

-   x86 -> x64
-   Code -> Asm / AsmCode
-   compile -> compileAndRun
-   codeBytes -> compile
-   Bytes -> MachineCode ?
-   Data -> DB
-   traceReg -> traceOp

# Documentation

-   add top level type signatures
-   more Haddock comments

# Other

-   merge Jmp and Jmpq constructors?

