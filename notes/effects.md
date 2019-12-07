Effect analysis proposal
========================

GRIN programs are pure by default, but certain external functions, such as primitive operations of the front end language can have side-effects. Knowing the possible side-effects for a given program is crucial for certain optimizing code transformations.

Motivation
----------

For example, a side-effecting computation can never be removed by a dead code eliminating optimization even if the computation's result is never used. See the following piece of GRIN code.

```haskell
grinMain =
  n <- pure 0
  x <- case n of
    0 -> _prim_int_print 0
    1 -> _prim_string_print "foo"
    2 -> pure ()
  pure 0
```

In this program, the variable `x` is never used, but the binding creating the variable contains side-effects, hence it cannot be removed.

Current solution
----------------

Currently, we only have a "limited" solution for calculating effect information available about a GRIN program. The algorithm interprocedurally calculates the side-effects associated with functions only (an `EffectMap`). This means, that it will not give any information about the binding creating the variable `x` in the above example. Now, we circumvent this problem by implementing an analysis that uses the `EffectMap` and calcualtes the effect information for bindings* as well.

This solution is quite hacky in the sense that the analysis is implemented in two passes, and the `EffectMap` is calculated in an ad hoc manner without using the abstract interpretation model used by every other analysis. This makes the code hard to understand.

(*): The left-hand side of a binding is always a variable, the variable names are unique, so a binding can be identified by the variable it binds.

Alternative solution
--------------------

By using the abstract interpretation model, we can easily implement an interprocedural effect tracking analysis, which produces the same result as the two above mentioned analyses combined. Furthermore, the implementation would fit into the framework more naturally, since it would use the same techniques as all the other analyses.

Below, you can see some of the more relevant rules of the analysis.

### Binding

```haskell
x <- <lhs>
<rhs>
```
- flow `<lhs>` into `x`
- flow `<lhs>` into the result of `<rhs>` (essentially flows data into the final return register of the bind sequence)
- return result of `<rhs>`

### Function definition

```haskell
f x =
  <body>
```
- flow the result of `<body>` into `f`'s result register
- return nothing

### Case expression

```haskell
case x of
  <alt 1> -> <rhs 1>
  ...
  <alt n> -> <rhs n>
```
- forall i in [1..n]. flow the result of `<rhs i>` into the case expression's result register
- return the result register of the case expression

### External application

```haskell
p x -- p is an external function
```
- create a new register `r`
- if `p` is effectful, then put `p`'s name into `r`
- return `r`

### Function application

```haskell
f x
```
- create a new register `r`
- flow `f`'s result into `r`
- return `r`

Some examples
-------------

### Simple

```haskell
grinMain =
  n <- pure 0
  x <- _prim_int_print 0
  pure 0
```

```haskell
Bindings
    n      -> {}
    x      -> {_prim_int_print}
Functions
    grinMain -> {_prim_int_print}
```

### Functions

```haskell
grinMain =
  n <- pure 0
  y <- f 0
  pure 0

f x =
  _prim_int_print 0
  pure x
```

```haskell
Bindings
    n      -> {}
    y      -> {_prim_int_print}
Functions
    f      -> {_prim_int_print}
    grinMain -> {_prim_int_print}
```

### Case expression

```haskell
grinMain =
  n <- pure 0
  x <- case n of
    0 -> _prim_int_print 0
    1 -> _prim_string_print "foo"
    2 -> pure ()
  pure 0
```

```haskell
Bindings
    n      -> {}
    x      -> {_prim_int_print
              ,_prim_string_print}
Functions
    grinMain -> {_prim_int_print
                ,_prim_string_print}
```

Some questions
--------------

### Simple dead variable elimination

Currently, `SDVE` uses the `EffectMap` to determine whether a function in an application node has any side effects or contains heap modifying operations. The new analysis does not track heap operations, but it would be easiy to incorporate that. However, the real question is whether `SDVE` really needs that information. Should `SDVE` use *any* interprocedural information?

Modifying heap operations do not have to be tracked, since the `store` and `update` operations have *evaluation* semantics. If we delete an `update`, but the heap location it evaluates is used somewhere else, then at the use-site there must be a call to `eval`, which means, we only delayed the evaluation of the thunk. Note that this assumption is only true in the current framework, with the current optimizations. We could thin of a strictness based optimization, which elminate the second evaluations, in which case, `SDVE` could produce semantically incorrect programs.

The question of whether `SDVE` should use any interprocedural information is postponed.

### Pattern bindings

As mentioned earlier, each binding should have a name, the variable it binds. Currently, the syntax allows for pattern bindings, which do not bind a variable to the left-hand side computation. This problem could be fixed by introducing @patterns into the syntax, and requiring each binding to have a name even if it is a pattern binding. Another solution could be to give an ID to each binding internally, when builing the AST. This would introduce some challanges for ID generation, and some inconvenience for the transformations.

In the future, we will define a new syntax for GRIN which will incorporate @patterns. For now, we will use a convention which can be assumed to hold for any GRIN program (can be guaranteed by applying a preliminary transformation). The convention states that any binding with a non-variable pattern must have a left-hand side of the form `pure <var>`. This ensures that every pattern has a name, but unfortunately the name and the pattern itself are not part of the same syntactic construct. As a consequence, some transformations will have to do some extra work (e.g.: Interprocedural Dead Variable Elimination).

#### @patterns


```haskell
v@(CInt k) <- pure n
<both v and k can be referenced here>
```

#### Temporary solution

```hakell
v <- pure n
(CInt k) <- pure v
<both v and k can be referenced here>
```
