# Version 0.4.5

-   fix build with newer base

# Version 0.4.4

-   export some useful data types 

# Version 0.4.3

-   fixes to compile on GHC 7.10 and on Windows

# Version 0.4.2

-   fix OSX compilation

# Version 0.4.1

-   fix a bug which caused segmentation fault if a compiled function was called multiple times

# Version 0.4

-   rewrite FFI
-   support calling labels
-   speed up long code alignments
-   bugfix: auto size calculation fix

# Version 0.3.1

-   use multi-byte nop operations for padding
-   `preBuild` operation (may speed up code generation)
-   branch-predicition friendlier if-then-else
-   not-condition pattern: `N`  
    usage example: `j (N E)` which is the same as `j NE`
-   bugfix: fix if-then-else condition

# Version 0.3

-   simpler API for label handling  
    (the idea of labels as De-Bruijn indices is abandoned)

# Version 0.2

-   simpler API for immediate values: `Add ax, 1`
-   redesigned addressing API: `Add ax (addr $ rax + 4*rdi + 3)`
-   support near jumps
-   support automatic decision between short and near jumps (conditional jumps too)
-   support more registers and instructions:
    -   cmovCC instructions
    -   SSE registers: xmm0 - xmm7
    -   SSE instructions: movd, movq, movdqa, paddb, paddw, paddd, paddq, psubb, psubw, psubd, psubq, pxor
    -   SSE instructions (partial support): psllw, pslld, psllq, pslldq, psrlw, psrld, psrlq, psrldq, psraw, psraq
-   bugfixes
    -   throw an error if an immediate value does not fit
    -   fix show instance of shift instructions

# Version 0.1.4

-   OS X operating system support (Balázs Kőműves)
-   better show for db
-   bugfix
    -   save flags in traceReg

# Version 0.1.3

-   jmpq instruction support (George Stelle)
-   support near conditional jumps
-   support automatic decision between short and near conditional jumps for backward references
-   support alternative condition names
-   make possible to use labels as relative immediate values (not used yet)
-   bugfix
    -   fail if a short jump is out of range

# Version 0.1.2

-   Windows operating system support (Balázs Kőműves)
-   GHC 7.10 support
-   TODO.md file added

# Version 0.1.1.1

-   change dependencies to reflect that the package compiles only with ghc 8
-   add 'tested-with: GHC == 8.0.1' to the cabal file

# Version 0.1.1

-   examples moved into the library
-   more Haddock comments
-   add cabal test suit
-   bugfixes
    -   fix code generation for alignments
    -   smaller code is generated now for 'add rax, 100' and similar instrcutions

