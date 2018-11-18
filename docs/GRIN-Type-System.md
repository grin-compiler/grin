# Notes on a possible type system for GRIN

LLVM is a typed intermediate language.
GRIN has to be typed to harness the full power of LLVM.
The GRIN type system must be expressive enough to
  - embed the LLVM type system
  - type the GRIN programs during transformations (at any stage)
  - describe the type of GRIN transformations (type preserving and well typed)

The result of the Heap-Points-To (HPT) analysis can be seen as a type environment.
In this setting the HPT analysis is the type inference algorithm.

e.g. HPT result for a GRIN program
``` haskell
Heap
    1      -> {CInt[{T_Int64}]}
    2      -> {CInt[{T_Int64}]}
    3      -> {CInt[{T_Int64}]
              ,Fadd[{1},{2}]}
Env
    a      -> {1}
    b      -> {2}
    b'     -> {T_Int64}
    m      -> {1}
    m'     -> {T_Int64}
    n      -> {2}
    n'     -> {T_Int64}
    q      -> {1,2,3}
    r'     -> {T_Int64}
    t1     -> {1}
    t2     -> {2}
    t3     -> {3}
    v      -> {CInt[{T_Int64}]
              ,Fadd[{1},{2}]}
    w      -> {CInt[{T_Int64}]}
    x'1    -> {T_Int64}
Function
    _prim_int_add :: {T_Int64}
                  -> {T_Int64}
                  -> {T_Int64}
    _prim_int_print :: {T_Int64}
                    -> {T_Unit}
    add :: {1}
        -> {2}
        -> {CInt[{T_Int64}]}
    eval :: {1,2,3}
         -> {CInt[{T_Int64}]
            ,Fadd[{1},{2}]}
    grinMain :: {T_Unit}
```

GRIN source program
``` haskell
grinMain =
  t1 <- store (CInt 1)
  t2 <- store (CInt 10000)
  t3 <- store (Fadd t1 t2)
  (CInt r') <- eval t3
  _prim_int_print r'

add m n =
  (CInt m') <- eval m
  (CInt n') <- eval n
  b' <- _prim_int_add m' n'
  pure (CInt b')

eval q =
  v <- fetch q
  case v of
    (CInt x'1) -> pure v
    (Fadd a b) -> w <- add a b
                  update q w
                  pure w
```

# Notes

A builtin dependent type function should be enough for full GRIN expressivity.
```
prj :: Tag -> Int -> a
```
OR
```
prjNode :: {Node A, ..., Node Z} -> Tag -> Node Tag
prjItem :: Node Tag -> Int -> a
```
