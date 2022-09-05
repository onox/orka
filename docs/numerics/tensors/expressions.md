# Expressions

While there exists many functions that operate on the individual elements
of a tensor, sometimes the elements of a tensor need to be combined in
some expression to produce a single element as the result.
For example, if one wants to create the sum of the numbers of a tensor
containing the values 1.0, 2.0, and 3.0, the expression 1.0 + 2.0 + 3.0
needs to be evaluated.

To do this, the package `:::ada Orka.Numerics.Tensors`
contains the interface type `Expression` to reduce the numbers in a tensor
using an arbitrary expression. An expression used in a reduction always
needs to involve two elements, represented by the functions `X` and `Y`.
These two functions return an `Expression` and can be used to further build
more complex expressions. For example, the expression above is evaluated
as (1.0 + 2.0) + 3.0.

The binary operators `+`, `-`, `*`, and `/`, and the functions `Min` and
`Max` can be used on two expressions or an expression and an element.
The unary operators `-` and `abs`, and the function `Sqrt` can only be used
on a single expression.

An expression can be constructed and then used repeatedly by the function
`Reduce` to reduce the elements of a tensor to a single element:

```ada
Expression_Sum : constant CPU_Expression := X + Y;
```

Note that `X` and `Y` are not parameters but functions, each returning
an `Expression`. The expression in `Expression_Sum` can then be used in
a reduction:

```ada
Count : constant Element := Object.Reduce (Expression_Sum, 0.0);
```

If an expression is associative, then function `Reduce_Associative` instead
of `Reduce` can be used.

The second parameter of the function `Reduce` contains the initial value of
the result and may be used as the actual parameter of one of the two arguments
when evaluating an expression. For example, when computing the sum, the
initial value should be 0.0, while computing the product requires the value 1.0.

There exist a few predefined functions that perform some common reductions
like the sum or product, and the minimum or maximum value in a tensor. These
are the functions `Sum`, `Product`, `Min`, and `Max`.

!!! summary
    A binary operation `*` is associative if for any `a`, `b`, and `c`,
    it is true that:

    - `:::ada a * (b * c) = (a * b) * c`
