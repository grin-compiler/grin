# Abstract Interpretation

Used as Heap Points To analysis implementation.

method: compiled abstract interpretation

## abstract values and locations

- heap location:  store count
- register:       register count (variables)
- node:           count of distinct nodes = count of distinct node tags
- simple type:    count of simple types (literal types)
- tag:            count of distinct tags

## articles
- [Abstracting Abstract Machines by David Van Horn and Matthew Might](https://vimeo.com/16539100) (video)
- [Systematic abstraction of abstract machines](http://matt.might.net/papers/vanhorn2012abstract.pdf)
- [Abstracting Definitional Interpreters](https://arxiv.org/pdf/1707.04755.pdf)
- https://en.wikipedia.org/wiki/Data-flow_analysis
- https://en.wikipedia.org/wiki/Abstract_interpretation

### points-to analysis
- [Pointer analysis overview (CMU)](http://www.cs.cmu.edu/afs/cs/academic/class/15745-s11/public/lectures/L27-Pointer-Analysis.pdf)
- [Pointer analysis overview (IAState)](http://web.cs.iastate.edu/~weile/cs513x/2.PointerAnalysis.pdf)
- [Lecture Notes: Pointer Analysis](https://www.cs.cmu.edu/~aldrich/courses/15-819O-13sp/resources/pointer.pdf)
- [Pointer Analysis Tutorial](https://yanniss.github.io/points-to-tutorial15.pdf)
- [Cloning-Based Context-Sensitive Pointer Alias Analysis Using Binary Decision Diagrams](https://suif.stanford.edu/papers/pldi04.pdf)
- [Points-to Analysis in Almost Linear Time](https://www.cs.cornell.edu/courses/cs711/2005fa/papers/steensgaard-popl96.pdf)
- [Fast and accurate flow-insensitive points-to analysis](http://www.cs.utexas.edu/users/pingali/CS380C/2007fa/papers/popl97.pdf)

### relation of dataflow analysis, abstract interpretation and type inference

- [Equivalence of data-flow analysis, abstract interpretation and type inference?](https://cs.stackexchange.com/questions/30746/equivalence-of-data-flow-analysis-abstract-interpretation-and-type-inference)
- [Types as Abstract Interpretations](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/cousot-types-as-abstract-interpretations.pdf)
- [Principles of Program Analysis](http://www.imm.dtu.dk/~hrni/PPA/ppa.html) book

### search for: abstract compilation
-  http://www.iro.umontreal.ca/~feeley/papers/BoucherFeeleyCC96.pdf ; Abstract compilation: A new implementation paradigm for static analysis
-  https://pdfs.semanticscholar.org/5ad8/cb6b477793ffb5ec29dde89df6b82dbb6dba.pdf ; A Graph–Free Approach to Data–Flow Analysis

### efficient implementation
- [EigenCFA: Accelerating Flow Analysis with GPUs](http://matt.might.net/papers/prabhu2011eigencfa.pdf)
- [A GPU Implementation of Inclusion-based Points-to Analysis](https://userweb.cs.txstate.edu/~mb92/papers/ppopp12.pdf)
- [Parallel Inclusion-based Points-to Analysis](http://iss.ices.utexas.edu/Publications/Papers/oopsla10-mendezlojo.pdf)

## Notes
  - HPT is liberal as much as possible; allow variadic case type (i.e. ANY -> ANY)

HPT is a program that fills a table. It calculates value sets for registers and heap locations.
The possible tags, simple types and heap locations are statically known for the input program.

HPT is a data flow analysis that can be implemented as an abstract machine.
The machine consists of:
  - memory: heap, registers ; store abstract values
  - operations: value setup with constant, value copy, conditional execution ; sequence of operations that models one iteration of a whole program data flow step

HINT: it is possible to execute some parts of the machine in parallel. (based on data dependency analysis)

HPT abstract value domain:

|               | node  | simple type | heap location |
| ---           | ---   | ---         | ---           |
| simple type   |       | simple type |               |
| heap location | node  |             |               |
| node          |       | simple type | heap location |
| register      | node  | simple type | heap location |

- node: `tag + {simple type | heap location}*`
- heap location: `node`
- register: `node | simple type | heap location`

The HPT can be performed only on high level GRIN.

High level GRIN:
  - fetch full nodes only
  - val: T a b | () | a | Lit
  - lpat: T a b | () | a | Lit
  - cpat: T a b | Lit

Conceptually the HPT analysis is done like the generic eval function was inlined.

## TODO

  - HPT IR LLVM codegen (HPT IR -> LLVM IR)
  - reduce temporary register usage
  - cleanup HPT IR codegen
  - add sharing analysis

### HPT IR improvements

- basic block support with block ID, useful for
  - one time variable intialization
  - tracking function body
  - if body
  - tracking of live or dead code

- batch `set` commands into one time running blocks

- one time run condition for `if`

- HPT IR inline support
  - scoped variables (with hierarchy support, i.e. block in block)
  - use explicit function block in IR
  - pass call site local variable context (that contains the function's variables) when calling inlined function

### Tooling improvements

- Debug support for HPT IR pure
  - [ ] log executed instructions (i.e. writer monad)
  - [x] debug instruction pretty printer that shows the readable variable names, simple types and node tags (i.e. `@1{name}`)

- Calculate GRIN statistics related to HPT performance
  - count of data constructors
  - count of `eval` calls
  - count of `stores`
  - count of GRIN variables

### Documentation

- Benchmark HPT for speed and memory usage
  - HPT IR LLVM
  - HPT IR pure
  - HPT abstract interpreter pure (with inline support)

## Compiled Abstract Interpretation

#### Abstract Machine

- abstract value: _set of (simple type | node type | location)_
- machine state:
  - finite set of registers: _register name -> abstract value_
  - bounded (abstract) memory == finite set of memory locations: _location -> abstract value_

#### Compile: Grin to Abstract Machine

The basic idea of abstract interpretation is to map each Grin variable to an abstract machine register which holds an abstract value.
An abstract value describes a value domain which can be interpreted as type.
The abstract value supports only a single operation: `union`. This means it can only be extended and never can be shrank.
The abstract value can be extended by a constant value (e.g. simple type, node type, location) or another abstract value.

The Grin memory operations (`store`, `update`, `fetch`) are altered to operate on the abstract machine's finite memory.
The mapping is done using the `store` operation. A unique memory location is assigned for each specific `store` operation that will be reused for the whole (abstract) program lifetime.

For each Grin function a set of abstract machine registers are assigned to hold the function's result type and argument type.
These registers are filled by the function body (return type) and the function callers (argument type).

The primitive functions do not have a function body. Their return type must be known (hard coded) to be able to fill the register that holds the return type.
For unknown primitive functions the return type will never be filled, which can affect other parts of the program via transitive dependency. (e.g. avoid execution)

Initially the abstract machine state is empty, meaning the abstract registers and memory locations are all mapped to empty sets.
The machine state gradually filled during the abstract interpretation.
It is possible that some registers or memory locations will remain untouched during the abstract interpretation.
These parts of the original program is statically known to be unused, it is dead code or dead parameter.
