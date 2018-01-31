# Differences to Boquist's PhD thesis RISC backend

- support for multiple types (float, int, etc.)
- LLVM is typed ; values must have proper types i.e.
- no register pinning ; instead pass registers (e.g. heap pointer) as local parameter
- rely on LLVM register allocator


Boquist's RISC backend only supports int type ; there are problematic expressions: unit (A Int | B Float)

IDEA:
  GRIN must be monomorph in LLVM type system
  Vectorisation is an efficient mapping to product types

node = tag + simple values + node pointers
con data = simple values + node pointers
slim layout: tag + con data
fat layout: tag + con1 data + ... + conN data

IDEA:
  support array literals
  support struct literals

HINT:
  fetch, store, update must be layout monorphic
  the only supported cast is pointer cast
