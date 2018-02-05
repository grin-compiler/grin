# Abstract Interpretation

Used as Heap Points To analysis implementation.

method: compiled abstract interpretation

## abstract values and locations

heap location:  store count
register:       register count (variables)
node:           count of distinct nodes = count of distinct node tags
simple type:    count of simple types (literal types)
tag:            count of distinct tags

## articles
  https://en.wikipedia.org/wiki/Data-flow_analysis
  https://en.wikipedia.org/wiki/Abstract_interpretation

  search for: abstract compilation
  http://www.iro.umontreal.ca/~feeley/papers/BoucherFeeleyCC96.pdf ; Abstract compilation: A new implementation paradigm for static analysis
  https://pdfs.semanticscholar.org/5ad8/cb6b477793ffb5ec29dde89df6b82dbb6dba.pdf ; A Graph–Free Approach to Data–Flow Analysis

## Notes
  - HPT is liberal as much as possible; allow variadic case type (i.e. ANY -> ANY)

HPT is a program that fills a table. It calcualtes value sets for registers and heap locations.
The possible tags, simple types and heap locations are statically known for the input program.

HPT is a data flow analysis that can be implemented as an abstract machine.
The machine consists of:
  - memory: heap, registers ; store abstract values
  - operations: value setup with constant, value copy, conditional execution ; sequence of operations that models one iteration of a whole program data flow step

HINT: it is possible to execute some parts of the machine in parallel. (based on data dependency analysis)

HPT abstract value domain:

| values        | node  | simple type | tag | heap location |
| ---           | ---   | ---         | --- | ---           |
| tag           |       |             |  x  |               |
| simple type   |       |  x          |     |               |
| heap location |  x    |             |     |               |
| node          |       |  x          |  x  |  x            |
| register      |  x    |  x          |  x  |  x            |

- node: `tag + {simple type | heap location}*`
- heap: `node`
- register: `node | simple type | tag | heap location`

