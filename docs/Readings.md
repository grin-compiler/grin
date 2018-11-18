# Readings

http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html

# GHC
### Core getting started

- [Dive into GHC: Pipeline](http://www.stephendiehl.com/posts/ghc_01.html)
- [Dive into GHC: Intermediate Forms](http://www.stephendiehl.com/posts/ghc_02.html)
- [Dive into GHC: Targeting Core](http://www.stephendiehl.com/posts/ghc_03.html)

### Internals

- [GHC (STG,Cmm,asm) illustrated for hardware persons](http://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf)
- [GHC Simplifier: Compilation by transformation for non-strict functional languages](https://www.microsoft.com/en-us/research/publication/compilation-transformation-non-strict-functional-languages)


# LLVM-HS

- [Calling External Functions from JIT-compiled LLVM Modules using llvm-hs](https://purelyfunctional.org/posts/2018-04-02-llvm-hs-jit-external-function.html)

# GRIN
- [Sharing Analysis + EVAL inlining + Unboxing = Deforestation](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.48.6217&rep=rep1&type=pdf)

# Recursion schemes

Resources for learning and using recursion schemes.

https://github.com/passy/awesome-recursion-schemes

## Thoughts

Limits of recusrion schemes? Maybe a hylo can help to implement the list filter?

- https://stackoverflow.com/questions/18421926/list-filter-using-an-anamorphism
- http://newartisans.com/2018/04/win-for-recursion-schemes

## Articles

- [Recursion Schemes: A Field Guide (Redux)](http://comonad.com/reader/2009/recursion-schemes/) -
  List of various recursion schemes with code samples.
- [Catamorphisms](https://wiki.haskell.org/Catamorphisms) - Definition on the Haskell Wiki.
- [Catamorphisms](https://www.schoolofhaskell.com/user/edwardk/recursion-schemes/catamorphisms) -
  Short definition with code on School of Haskell by Edward Kmett.
- [Rotating Squares](https://jtobin.io/rotating-squares) - Using a hylomorphism to rotate a quadtree by Jared Tobin.
- [Promorphisms, Pre and Post](https://jtobin.io/promorphisms-pre-post) - Pratical examples of pre- and postpromorphisms by Jared Tobin.
- [Time Traveling Recursion Schemes](https://jtobin.io/time-traveling-recursion) - Exploring histo and futu by example by Jared Tobin.
- [Correcting the Visitor pattern](http://logji.blogspot.co.uk/2012/02/correcting-visitor-pattern.html) - Showing that the Visitor pattern implements an f-algebra for use with a catamorphism (in Java).
- https://stackoverflow.com/questions/36851766/histomorphisms-zygomorphisms-and-futumorphisms-specialised-to-lists

## Cheat Sheet

http://b-studios.de/blog/2016/02/21/the-hitchhikers-guide-to-morphisms/

### Recursion schemes

<img src="images/recursion-schemes-cheat-sheet.svg" style="max-width: 100%; max-height: 100vh; height: auto;">

### Algebras

<img src="images/algebras.svg" style="max-width: 100%; max-height: 100vh; height: auto;">
