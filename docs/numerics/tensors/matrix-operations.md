# Matrix operations

2-D tensors, or matrices, have a few special operations that can be
applied to them.

## Matrix multiplication

The `*` operator is used to perform matrix multiplication on matrices
and vectors. Matrix multiplication is useful to apply transformations
to points (vectors). For example, `:::ada A * B * X` transforms `X`
by `B` to B **x** and then multiplies with `A` to transform it to the
result A B **x**.

!!! tip
    It is more efficient to compute `:::ada A * (B * X)` than
    `:::ada A * B * X` if `X` is a column. This is because the latter
    will perform one matrix-matrix and one matrix-vector multiplication,
    while the former will perform two matrix-vector multiplications, but
    zero matrix-matrix multiplications.

!!! warning "Use function `Multiply` for element-wise multiplication"

!!! summary
    Matrix multiplication is associative and distributive:

    - `:::ada A * (B * C) = (A * B) * C`

    - `:::ada A * (B + C) = A * B + A * C` (similar for `:::ada (B + C) * A`)

!!! warning "Matrix multiplication is not commutative"
    Matrix multiplication is not commutative: `:::ada A * B /= B * A`.

## Matrix power and inverse

If the left operand of the operator `**` is a tensor and the right operand
an `Integer`, then the operator will perform the matrix power. That is,
`:::ada A ** K` where `A` is a tensor, will multiply the matrix `A` with
itself `K` times. For example, `:::ada A ** 3 = A * A * A`.

Note that matrix `A` must be square. `:::ada A ** 0` is equal to the
identity matrix and `:::ada A ** (-1)` is equal to the inverse of `A`.
Any `K` lower than -1 will first compute the inverse of `A` and then
raise that to the power of the absolute value of `K`.

Alternatively, the function `Inverse` will perform `:::ada A ** (-1)`.

!!! warning "An exception is raised if the matrix is not invertible"
    If a matrix `A` is singular, the inverse does not exist and function
    `Inverse` or the `**` operator will raise the exception `Singular_Matrix`.

!!! note
    A **x** = **b** can be solved for **x** with A^-1^ **b**, but it is
    more efficient and accurate to use function `Solve`.

!!! summary
    The inverse of `:::ada A * B` is equal to the product of the
    inverses in the reverse order:
    `:::ada (A * B).Inverse = B.Inverse * A.Inverse`.

    The inverse of an invertible matrix `A` is also invertible:

    - `:::ada A.Inverse.Inverse = A`

## Transpose

The function `Transpose` returns the transpose of a 2-D tensor; the columns
become the rows and the rows become the columns.

For example, given a 2 × 3 matrix `Tensor` containing the following elements:

```
tensor([[ 1.0, 2.0, 3.0],
        [ 4.0, 5.0, 6.0]])
```

The image of the transpose `:::ada Tensor.Transpose` will print:

```
tensor([[ 1.0, 4.0],
        [ 2.0, 5.0],
        [ 3.0, 6.0]])
```

!!! summary
    The transpose of `:::ada A * B` is equal to the product of the
    transposes in the reverse order:
    `:::ada (A * B).Transpose = B.Transpose * A.Transpose`.

    Other properties that apply are:

    - `:::ada A.Transpose.Transpose = A`

    - `:::ada (A + B).Transpose = A.Transpose + B.Transpose`

## Outer product

The outer product of two vectors, 1-D tensors, returned by the function
`Outer`, is a matrix, a 2-D tensor. The number of rows is equal to the
size of the first vector and the number of columns is equal to the size
of the second vector.

!!! info "The difference between the outer and inner products"
    The outer product for two vectors **u** and **v** is defined as
    **u** **v**^T^ and is a *n* × *m* matrix, while the inner product
    (or dot product) is defined as **u**^T^ **v** is a 1 × 1 matrix.

## Solving A **x** = **b**

A system of linear equations can be solved by row reducing the augmented
matrix [A **b**] to [I **x**], where I is the identity matrix.
The function `Solve` is given two tensors `A` and `B` and solves A **x** = **b**
for each vector **b** in tensor `B`.

The function has a third parameter `Solution` of mode `out`. It will have
one of the following values after the function `Solve` returns:

- `None`. If the system is inconsistent for any vector in `B`.

- `Infinite`. If `A` has more columns than rows or if one of its pivots is
  not on the main diagonal, then `A` has one or more free variables and
  thus the system of linear equations will have an infinite number of solutions.
  If `A` has free variables, their vectors are not returned in any `out` parameter.

- `Unique`. A **x** = **b** has exactly one solution for all vectors in `B`.

!!! tip
    If you do not know whether the system is always consistent with a
    unique solution, then it is recommended to just call the function `Least_Squares`
    instead (see below).
    It will return a solution even if **b** is not in the column space of A.

## Trace

The trace of a matrix or 2-D tensor is the sum of the elements on the main
diagonal and can be computed with the function `Trace`.
The optional parameter `Offset` can be given to specify the diagonal to use

## Main diagonal

Function `Main_Diagonal` returns a 1-D tensor filled with the elements on
the main diagonal of a 2-D tensor. The diagonal to use can be specified with
the optional parameter `Offset`.

## Least-squares

The equation A **x** = **b** describes the transformation of a vector
**x** to vector **b**. If the system of equations is consistent, then
**b** is a linear combination of the columns of A.

If a matrix A has the shape *m* × *n* with *m* < *n*, then A has
free variables and A **x** = **b** has an infinite number of solutions;
there exist multiple vectors **x** that can be transformed to vector **b**.
To say it differently: vector **b** is in the column space of A, the set
of all linear combinations of the columns of A.

If *m* > *n* instead, then row reducing [A **b**] to [U **x**] may result
in the last few rows of U containing zeros. In that case the system of equations
is inconsistent and there is no solution. At most there is one solution
because there are no free variables. Thus there may exist a vector **b**
that is not in the column space of A.

For example, the identity matrix I with the shape 3 × 3 has pivots in each
column and thus the columns are linear independent and span the infinite
column space of I: you can think of any vector **x** (a point in a 3-D space)
and I **x** will be equal to **x** itself.

Another matrix, A, may have the shape 3 × 2, and if the two columns are
not a multiple of each other, then they span a 2-D plane in a 3-D space ℝ^3^.
A vector **b** in ℝ^3^ (with three elements) in the column space of A is a
linear combination of the columns of A and the vector **x** in ℝ^2^
(a vector with two elements), which is a point in the plane spanned by
the columns of A.

Now imagine that the columns of matrix A create a horizontal plane and vector
**b** is a point floating just above this plane. Then **b** is not in the
column space of A and the equation A **x** = **b** has no solution.
However, there may be vector A **x'** = **b'** that is in the column space of A
and that is very close to **b**. Thus the least-squares problem is finding
a vector **x'** that minimizes the distance between **b** and A **x'**.

Given a matrix `A`:

```ada
A : constant CPU_Tensor :=
  To_Tensor ((1.0,  5.0,
              1.0, -2.0,
              1.0, -4.0,
              1.0,  1.0)).Reshape ((4, 2));
```

and a vector `B`:

```ada
B : constant CPU_Tensor := To_Tensor ((2.0, 3.0, -3.0, 7.0));
```

Function `Least_Squares` can be used to compute the least-squares
solution as follows:

```ada
X : constant CPU_Tensor := Least_Squares (A, B);
```

The image of `X` will be:

```
tensor([ 2.25000E+00, 5.00000E-01])
```

If the columns of `A` are orthogonal (which happens to be the case
in the example above), then the orthogonal projection of `B` onto
column **a**~i~ of `A` is a linear combination of the column **a**~i~
and the coefficient (**b** ∙ **a**~i~) / (**a**~i~ ∙ **a**~i~).
This is an alternative way to compute the values of **x** and does not
require computing the QR factorization of A.

For the example above, **x** is 9.0 / 4.0 and 23.0 / 46.0.

### Caching the QR factorization

The function `Least_Squares` above computes the QR factorization
needed to compute the least-squares solution. If a solution must be
computed repeatedly for different vectors **b**, then it is helpful
to cache the QR factorization using the function `QR_For_Least_Squares`:

```
QR_A : constant CPU_QR_Factorization :=
  CPU_QR_Factorization (QR_For_Least_Squares (A));
```

If `A` is undetermined (*m* < *n*) then function `QR_For_Least_Squares`
computes the QR factorization of A^T^ instead.
There is another function `QR` that does *not* perform this automatic
transposition, but the resulting object cannot be used by function
`Least_Squares`. In fact, doing so will raise a `Program_Error`.

The factorization `QR_A` can then be used by function `Least_Squares`
to compute the least-squares solution for some tensor `B`:

```ada
X : constant CPU_Tensor := Least_Squares (QR_A, B);
```

Tensor `B` can be a vector or a matrix of column vectors.

### Orthogonal projection

The orthogonal projection **b'** of **b** onto the column space of A
can be obtained by multiplying `A` with the computed least-squares
solution `X` or by computing Q Q^T^ **b** where Q is the
orthogonal matrix Q from the QR factorization of `A`.

### Adding constraints

The function `Constrained_Least_Squares` can be used to constrain
**x'** such that C **x'** = **d** when computing the least-squares
solution **x'**:

```ada
X : constant CPU_Tensor := Constrained_Least_Squares (A, B, C, D);
```

Tensor `C` must be a matrix and have the same number of columns as `A`
and same number of rows as `D`. Tensor `B` and `D` must have same number
of columns.

The smallest **x'** can be computed for which C **x'** = **d** by setting
`A` to the identity matrix and `B` to a vector of zeros.
Minimizing ‖A **x** - **b**‖^2^ becomes ‖**x**‖^2^:

```ada
Size : constant Natural := C.Shape (2);

I    : constant CPU_Tensor := Identity (Size => Size);
Zero : constant CPU_Tensor := Zeros (Elements => Size);

Min_Value : constant Element := Constrained_Least_Squares (I, Zero, C, D).Norm**2;
```
