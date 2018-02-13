# Notes on a possible type system for GRIN

LLVM is a typed intermediate language.
GRIN has to be typed to harness the full power of LLVM.
The GRIN type system must be expressive enough to
  - embed the LLVM type system
  - type the GRIN programs during transformations (at any stage)

The result of the Heap-Points-To (HPT) analysis can be seen as a type environment.
In this setting the HPT analysis is the type inference algorithm.

i.e. HPT result for a GRIN program

HPT result
```
Heap
    1      -> {CCons[{1,5},{6}]
              ,CInt[{T_Int64}]
              ,CNil[]}
    2      -> {CCons[{1,5},{6}]
              ,CInt[{T_Int64}]
              ,CNil[]}
    3      -> {CCons[{1,5},{6}]
              ,CInt[{T_Int64}]
              ,CNil[]
              ,Fupto[{1},{2}]}
    4      -> {CCons[{1,5},{6}]
              ,CInt[{T_Int64}]
              ,CNil[]
              ,Fsum[{3}]}
    5      -> {CCons[{1,5},{6}]
              ,CInt[{T_Int64}]
              ,CNil[]}
    6      -> {CCons[{1,5},{6}]
              ,CInt[{T_Int64}]
              ,CNil[]
              ,Fupto[{5},{2}]}
Env
    a      -> {1,5}
    ax'    -> {T_Int64}
    b      -> {2}
    b'     -> {T_Bool}
    c      -> {3}
    l      -> {3,6}
    l2     -> {CCons[{1,5},{6}]
              ,CInt[{T_Int64}]
              ,CNil[]
              ,Fsum[{3}]
              ,Fupto[{1,5},{2}]}
    m      -> {1,5}
    m'     -> {T_Int64}
    m1     -> {5}
    n      -> {2}
    n'     -> {T_Int64}
    p      -> {6}
    q      -> {1,2,3,4,5,6}
    r'     -> {T_Int64}
    s'     -> {T_Int64}
    t1     -> {1}
    t2     -> {2}
    t3     -> {3}
    t4     -> {4}
    v      -> {CCons[{1,5},{6}]
              ,CInt[{T_Int64}]
              ,CNil[]
              ,Fsum[{3}]
              ,Fupto[{1,5},{2}]}
    w      -> {CCons[{1,5},{6}]
              ,CNil[]}
    x      -> {1,5}
    x'     -> {T_Int64}
    x'1    -> {T_Int64}
    xs     -> {6}
    y      -> {1,5}
    ys     -> {6}
    z      -> {CInt[{T_Int64}]}
Function
    _prim_int_add :: {T_Int64}
                  -> {T_Int64}
                  -> {T_Int64}
    _prim_int_gt :: {T_Int64}
                 -> {T_Int64}
                 -> {T_Bool}
    _prim_int_print :: {T_Int64}
                    -> {T_Unit}
    eval :: {1,2,3,4,5,6}
         -> {CCons[{1,5},{6}]
            ,CInt[{T_Int64}]
            ,CNil[]
            ,Fsum[{3}]
            ,Fupto[{1,5},{2}]}
    grinMain :: {T_Unit}
    sum :: {3,6}
        -> {CInt[{T_Int64}]}
    upto :: {1,5}
         -> {2}
         -> {CCons[{1,5},{6}],CNil[]}
```

GRIN source program
```
grinMain =
  t1 <- store (CInt 1)
  t2 <- store (CInt 10000)
  t3 <- store (Fupto t1 t2)
  t4 <- store (Fsum t3)
  (CInt r') <- eval t4
  _prim_int_print r'

upto m n =
  (CInt m') <- eval m
  (CInt n') <- eval n
  b' <- _prim_int_gt m' n'
  case b' of
    True -> pure (CNil)
    False -> m1' <- _prim_int_add m' 1
             m1 <- store (CInt m1')
             p <- store (Fupto m1 n)
             pure (CCons m p)

sum l =
  l2 <- eval l
  case l2 of
    (CNil) -> pure (CInt 0)
    (CCons x xs) -> (CInt x') <- eval x
                    (CInt s') <- sum xs
                    ax' <- _prim_int_add x' s'
                    pure (CInt ax')

eval q =
  v <- fetch q
  case v of
    (CInt x'1) -> pure v
    (CNil) -> pure v
    (CCons y ys) -> pure v
    (Fupto a b) -> w <- upto a b
                   update q w
                   pure w
    (Fsum c) -> z <- sum c
                update q z
                pure z
```
