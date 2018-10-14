# GRIN - LLVM - WebAssemby Experiment

## Run
0. clone this repo
```bash
cd grin/grin/wasm-sum-test
firefox grin-wasm-opt.html
firefox grin-wasm-simple.html
```
![](https://raw.githubusercontent.com/grin-tech/grin/webassemby-experiment/grin/grin/wasm-sum-test/wasm-sum-simple-output.png)

### GRIN optimization OFF
Thunks and data are stored on heap.

![](https://raw.githubusercontent.com/grin-tech/grin/webassemby-experiment/grin/grin/wasm-sum-test/wasm-grin-simple.png)


### GRIN optimization ON

No heap usage.

![](https://raw.githubusercontent.com/grin-tech/grin/webassemby-experiment/grin/grin/wasm-sum-test/wasm-grin-opt.png)

## Sample Program
Sample Haskell program
```
main = print $ sum [1..100]
```

GRIN representation

```haskell
grinMain = t1 <- store (CInt 1)
           t2 <- store (CInt 100)
           t3 <- store (Fupto t1 t2)
           t4 <- store (Fsum t3)
           (CInt r') <- eval t4
           _prim_int_print r'

upto m n = (CInt m') <- eval m
           (CInt n') <- eval n
           b' <- _prim_int_gt m' n'
           if b' then
            pure (CNil)
           else
            m1' <- _prim_int_add m' 1
            m1 <- store (CInt m1')
            p <- store (Fupto m1 n)
            pure (CCons m p)

sum l = l2 <- eval l
        case l2 of
          (CNil)       -> pure (CInt 0)
          (CCons x xs) -> (CInt x') <- eval x
                          (CInt s') <- sum xs
                          ax' <- _prim_int_add x' s'
                          pure (CInt ax')

eval q = v <- fetch q
         case v of
          (CInt x'1)    -> pure v
          (CNil)        -> pure v
          (CCons y ys)  -> pure v
          (Fupto a b)   -> w <- upto a b
                           --update q w -- FIXME: enable sharing analysis
                           pure w
          (Fsum c)      -> z <- sum c
                           --update q z -- FIXME: enable sharing analysis
                           pure z
```
