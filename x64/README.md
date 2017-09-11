# x86-64

The primary goal of x86-64 is to provide a lightweight assembler for machine generated 64 bit x86 assembly instructions.

Features:

-   The size of operands are statically checked. For example, exchanging `rax` with `eax` raises a compile time error rather than a code-generation time error. As a consequence, code generation is faster because the sizes are statically known.
-   Quickcheck tests: You can quickcheck your x86 processor! Please report failures, there is a higher chance that the error is in this library rather than in your processor.
-   Immediate values are automatically converted to smaller size if possible.
-   Automatic calculation of short and near distances

The package is available on HackageDB: http://hackage.haskell.org/package/x86-64bit

