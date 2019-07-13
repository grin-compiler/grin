Liveness and Effects
====================

The liveness of a variable is a property that describes whether the variable is needed for the final output of a program. Liveness information can be used to determine which parts of the program are actually needed, and which ones can be safely removed, thus eliminating unnecessary computations.

Pure liveness and side effects
------------------------------

If we assume all programs to be pure, then the only output of a program will be a single return value. In this case a variable is live if and only if it is needed for the computation of the final return value. However, most programs contain side effects, which means the output of the program is not a single return value, but some sort of state as well (such as IO). As a consequence, a variable may be needed for the output of a program, even though it is not needed for the return value.

The problem
-----------

In theory, it would be beneficial to separate the pure liveness side effects, but in practice they are much easier to work with together. Look at the following GRIN program.

```haskell
grinMain =
  a <- pure 0
  b <- pure 0
  c <- pure b
  d <- case c of
        0 ->
          e <- _prim_int_add a 0
          _prim_int_print 0
  pure 0
```

Here, the variable `d` is not needed for the return value of the program, but there is a side-effecting computation bound to it. The computation is a case expression with a variable scrutinee named `c`. The case expression cannot be removed because it contains a side-effecting external, which means `c` cannot be removed either. To further complicate matters, `c` is given in terms of `b`, hence `b` must stay as well. The expected output is the code below.

```haskell
grinMain =
  b <- pure 0
  c <- pure b
  d <- case c of
        0 ->
          _prim_int_print 0
  pure 0
```

Current implementation
----------------------

Currently, the liveness analysis and the effect analysis are implemented seperately. This means, the liveness analysis only considers pure computations (*), and the effects are calcualted separately. As a consequence, a variable deemed DEAD by only considering the pure result of the program (liveness analysis), may very well be LIVE if we also consider side effects (side effect tracking).

During dead code elimination, we only have access to the results of these analyses, but not to the dependecies between the variables. This is a huge problem, because the liveness based on side effects is not propagated back to the variables the given expression depends on.

Considering the original example, we have both liveness and effect information about all the variables. For example, `d` is DEAD, because its value is irrelevant to the return value of the program, but the expression bound to it contains side effects. By combining these information, we can conclude that `d`'s binding cannot be removed, but this information is not propagated back to `c`. `c` was also deemed DEAD by the liveness analysis, but if `d` was live, so should be `c`, because it is the scrutinee of the case expression, whose result is bound to `d`. Also, `c`'s liveness is not propagated back to `b` either. So, the dead variable transformation would give a semantically incorrect program.

```haskell
grinMain =
  d <- case (#undefined :: T_Int64) of
          0 ->
            _prim_int_print 0
  pure 0
```

(*): Tracking side effects during liveness analysis would considerably increase the complexity of the analysis. LVA's implementation is already quite complicated.

The real problem
----------------

If we closely inspect the syntax of GRIN, we can conlude that the only problematic left-hand side for a binding is a case expression.

- heap operations cannot contain side effects
- function applications are handled correctly (**)
- `pure`s are handled correctly

So the only problematic left-hand sides are case expressions. They are problematic, because the case expression's result is dependent on the scrutinee. Any side effecting computation in any of the alternatives implicitly effects the liveness of the scrutinee, because the scrutinee's value determines which alterntive will be executed. In other words, the value of the scrutinee determines whether there can be a side effect during the execution of the program. This means, the liveness information of the result of the case expression should be set LIVE if any possible alternatives contain side effects. Furthermore, this infomration should be propagated back to the scrutinee.

(**): The information is correctly propagated inside functions. If an argument is dead, then the function's return value is not dependent on it, and neither does the function contain any side effects dependent on that argument.

Alternative solution
--------------------

Instead of calculating the liveness information based on only the pure result of the program, we could utilize effect information too. Using this extra information, LVA would be able to properly propagate liveness information about "side effecting variables".

The actual modification would be quite simple. We would only need to add an optional parameter for the effect tracking result (`Maybe ETResult`), and we would have to mark all variables LIVE having any side effects associated with them. The propagation of this liveness information would be done automatically by the analysis.
