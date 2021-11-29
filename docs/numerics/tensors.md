# Tensors

Tensors are multidimensional arrays of numbers on which element-wise
operations such as arithmetic or trigonometry can be performed.
Some tensors, those which are 2-D, support special matrix operations
like matrix multiplication, computing the inverse or transpose, or
solving a set of linear equations.

Furthermore, tensors can be created with a specific statistical distribution
or reduced to a single number with basic arbitrary expressions
(for example, to compute the sum or product).

Currently there is one implementation which runs on the CPU using x86 SIMD
intrinsics, but a future different implementation could use GPU buffers
instead.

The SIMD implementation has a few restrictions:

- Numbers in tensors are always floating-point numbers

- No pointers are used; functions always return a new tensor and
  do not modify the tensor parameters of a function.

Tensors provide the following features:

- Constant indexing using a number, range, or another (boolean) tensor
- Matrix multiplication and power
- Inverse, transpose, outer product
- Solve A **x** = **b**
- Constructors to create tensors from arrays, filled with zeros or ones
- Constructors to create a range of numbers, or linear/log/geom space
- Constructors to create an identity matrix, or with a diagonal
- Reshape, flatten, or concatenate tensors
- Element-wise operations (arithmetic, rounding, trigonometry, compare)
- Reductions using arbitrary expressions
- Statistics (operations and generating statistical distributions)
- Logical operations on boolean tensors

??? bug "Limitations of tensors"
    All tensors have the following limitations:

    - Tensors of three dimensions or higher are currently not supported.
      Element-wise operations do not need modifications, but other functions
      may need to be modified to handle 3-D tensors.

    - Numbers in tensors are always floating-point numbers because of the
      generic parameter of the package `:::ada Orka.Numerics.Tensors`.

    - New values cannot be assigned to specific parts of a tensor using
      non-constant indexing.

    - Special matrix decompositions are not implemented yet.

## Shape and dimensions

Each tensor has a shape, the number of elements in each dimension,
of the type `Tensor_Shape`; an indefinite array of natural numbers.
For example, the shape of a 2 x 3 matrix is `:::ada (2, 3)` and the shape
of a vector of 100 elements is `:::ada (1 => 100)`.

The shape of a tensor can be retrieved using the function `Shape` and
the dimensions with the function `Dimensions`. The total number of
elements in the tensor is queried with the function `Elements`.

It is true that `:::ada T.Shape'Length = T.Dimensions` and
`:::ada Elements (T.Shape) = T.Elements` for a tensor `T`.

##  Image

The image of a tensor can be obtained with the function `Image`:

```ada
Orka.OS.Put_Line (Tensor.Image);
```

##  Element-wise operations

All tensors, no matter which number of dimensions they have, support
the many element-wise operations in `:::ada Orka.Numerics.Tensors`,
using the operators provided by the Ada language.
Binary operators support operations on two tensors as well as one tensor
and one element.

### Arithmetic

The following binary operators can be used on tensors or a tensor and
an element: `+`, `-`, `*`, `/`, `**`, `mod`, and `rem`. Unary operators
like `-` and `abs` can be used on a single tensor.

Be warned that `*` does not perform element-wise multiplication if used
on two tensors and `**` if used on a tensor and an `Integer` exponent.
Besides using operators, some of the operations can also be performed
with the functions `Add`, `Subtract`, `Multiply`, `Power` (uses a single
`Integer` as the exponent), and `Divide`.

The `/` operator may return NaNs if the denominator is zero.
The function `Divide_Or_Zero` can be used to create zeros at these places
instead.

!!! warning "`*` and `**` are also used as operators for matrices"
    The `*` operator performs matrix multiplication if both operands
    are tensors. Use the function `Multiply` for element-wise multiplication.
    The `**` operator provides the matrix power operation if the
    base is a tensor and the exponent an `Integer`. Use the function `Power`
    to raise the elements of a tensor to the power of a given `Integer`.

For example, given two tensors, the elements of the second tensor can be
multiplied with a number and then added to the first tensor to create
a new third tensor:

```ada
Tensor_3 : constant CPU_Tensor := Tensor_1 + 2.0 * Tensor_2;
```

Another example showing the `abs` and `mod` operators:

```ada
Tensor_4 : constant CPU_Tensor := (abs Tensor_1) mod Tensor_2;
```

### Rounding

To round numbers up, down, or the nearest integral value, the function
`Ceil`, `Floor`, or `Round` can be used. To truncate the floating-point
numbers, use the function `Truncate`.

### Math

The square-root can be used obtained with the function `Sqrt`.
The operation *e*^x, where x are the elements of a tensor, can
be performed using the function `Exp`. The natural logarithm with
the function `Log`, and the base 10 and base 2 logarithms with `Log10`
and `Log2`.

A tensor containing elements that are the minimum or maximum of a
pair of elements can be created with the functions `Min` and `Max`.
These two functions can operate on two tensors or a tensor and a single
element.

### Trigonometry

The sine, cosine, or tangent (in radians) can be computed with the
functions `Sin`, `Cos`, and `Tan`.

The arc sine, arc cosine, and arc tangent can be computed with the
functions `Arcsin`, `Arccos`, and `Arctan`. Values must be
in the range -1.0 .. 1.0 for `Arcsin` and `Arccos`, otherwise an exception
may be raised.
The function `Arctan` has two parameters and both must be greater than zero.

The function `Degrees` can be used to convert elements in a tensor from
radians to degrees. Function `Radians` converts elements from degrees to
radians.

### Logical operations

Several operators like `and`, `or`, `xor`, and `not` can be used to compare
the elements of two tensors. The function `And_Not` is equal to
`(not Left) and Right`. The operator `not` inverts the boolean values in the
tensor.

All of the operators (except for `and`) require the two tensors
to be boolean tensors and the tensor they return is also a boolean tensor.
A boolean tensor can be created by using one of the comparison operators (see below).

As mentioned earlier, the left operand of the `and` operator does not need to
be a boolean tensor; it can be a tensor containing floating-point elements or
even be a single element. The tensor this operator returns has the same type as the
left operand and its values will be the values of the left operand whenever the
corresponding (boolean) value of the right operand (always a tensor) is `True`.
if the corresponding value is `False` then the value in the returned tensor is either
0.0 or `False`.

The `and` operator is very useful to select elements based on the truth value
of the right tensor. For example, to select values from a tensor `Tensor_Values`
using the boolean tensor `Is_Valid`:

```ada
Tensor_Valid_Values : constant CPU_Tensor := Tensor_Values and Is_Valid;
```

To increment the elements of a tensor `Counter` when the corresponding boolean in
a tensor `Is_Match` is `True`, write:

```ada
Counter := Counter + (1.0 and Is_Match);
```

In these two examples, the boolean tensor of the right operand acts as a mask to
select elements.

### Comparing

Boolean tensors can be created by using Ada's comparison operators:
`=`, `/=`, `>`, `<`, `>=`, and `<=`. All of these operators can compare two
tensors or a tensor and a single element. The tensors involved in the comparison
must not be boolean tensor but a regular tensor.

For example, the function `Binomial` iteratively updates a tensor `Result` as
follows:

```ada
Result := Result + (1.0 and (Uniform (Shape) <= P));
```

where `P` is an element and `Uniform` a function that generates a tensor with
a uniform distribution. This example shows a comparison, a logical operation,
and arithmetic, all in one line of code, even though the tensors involved
might contain hundreds of thousands of elements depending on `Shape`.

Additionally, the `=` operator can return a `Boolean`, which will be `True` if
all values of the two tensors are equal, or `False` otherwise:

```ada
if Tensor_1 = Tensor_2 then
    Orka.OS.Put_Line ("The two tensors are equal");
end if;
```

The function `All_Close` can be used to test if two tensors are equal
enough given a relative and absolute tolerance.

After obtaining a boolean tensor, the tensor can be used in logical operations
or used as an index (see below) to return a new (smaller) tensor consisting
of only the elements selected by the index.

The functions `Any_True` and `All_True` return `True` if any or all elements
a boolean tensor are `True`, and `False` otherwise. In particular, `Any_True`
and `All_True` are useful to exit a loop:

```ada
exit when not Any_True (Loop_Condition);
```

This is useful in algorithms where you want to update elements of a tensor
iteratively and exit the loop once all elements meet or no longer meet some
condition.

## Reductions using arbitrary expressions

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

The second parameter of the function `Reduce` contains the initial value of
the result and may be used as the actual parameter of one of the two arguments
when evaluating an expression. For example, when computing the sum, the
initial value should be 0.0, while computing the product requires the value 1.0.

There exist a few predefined functions that perform some common reductions
like the sum or product, and the minimum or maximum value in a tensor. These
are the functions `Sum`, `Product`, `Min`, and `Max`.

## Indexing

To get elements at a certain index, tensors support Ada 2012's indexing
syntax. Several use cases are supported:

- Retrieving a single element of a 1-D or 2-D tensor, or a single row of
  a 2-D tensor.

- Retrieving a range of elements (1-D) or a range of rows (2-D).

- Retrieving a slice of elements consisting of multiple rows and multiple columns.

- Retrieving a number of elements selected using a boolean tensor.

### A row or value

A single element or boolean can be retrieved if the tensor is 1-D:

```ada
Some_Element : constant Element := Tensor (I);
```

where `I` is some `Positive` number.

If the tensor is 2-D, then a 1-D tensor is returned containing the elements
of the requested row:

```ada
Row_I : constant CPU_Tensor := Tensor (I);
```

If the tensor is 2-D and the index is a `Tensor_Index` then it returns the
`Element` or `Boolean` at the given index:

```ada
Some_Element : constant Element := Tensor ((I, J));
```

### Multiple rows and columns

If the given index is a `Range_Type` then the elements or rows at the given
range is returned depending on whether the tensor is 1-D or 2-D:

```ada
Some_Rows : constant CPU_Tensor := Tensor (Range_Type'(Start => 5, Stop => 10));
```

If one wants to extract multiple rows and/or multiple columns, an index of
the type `Tensor_Range` can be used. For example, given a 4 x 4 matrix,
the last two rows and the last three columns can be retrieved as follows:

```ada
Sub_Matrix : constant CPU_Tensor := Tensor (Tensor_Range'((3, 4), (2, 4)));
```

### Using a boolean tensor

Another way to retrieve elements of a tensor, is to use another (boolean)
tensor as the index. Each element in the tensor for which the boolean tensor
is `True` is stored in the returned tensor. The returned tensor is always 1-D,
no matter the number of dimensions of the original tensor.

For example, given a 2 x 3 matrix `Tensor` containing the following elements:

```
array([[ 1.0, 2.0, 3.0],
       [ 4.0, 5.0, 6.0]])
```

Then `:::ada Tensor (Tensor > 4.0).Image` prints the following:

```
array([ 5.0, 6.0])
```

And `:::ada Tensor (Tensor mod 2.0 = 0.0).Image` will print:

```
array([ 2.0, 4.0, 6.0])
```

## Matrix operations

2-D tensors, or matrices, have a few special operations that can be
applied to them.

### Matrix multiplication

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

!!! warning "Matrix multiplication is not commutative"
    Matrix multiplication is not commutative: `:::ada A * B /= B * A`.

!!! summary
    Matrix multiplication is associative and distributive:

    - `:::ada A * (B * C) = (A * B) * C`

    - `:::ada A * (B + C) = A * B + A * C` (similar for `:::ada (B + C) * A`)

### Matrix power and inverse

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
    A **x** = **b** can be solved for **x** with A^-1 **b**, but it is
    more efficient and accurate to use function `Solve`.

!!! summary
    The inverse of `:::ada A * B` is equal to the product of the
    inverses in the reverse order:
    `:::ada (A * B).Inverse = B.Inverse * A.Inverse`.

    The inverse of an invertible matrix `A` is also invertible:

    - `:::ada A.Inverse.Inverse = A`

### Transpose

The function `Transpose` returns the transpose of a 2-D tensor; the columns
become the rows and the rows become the columns.

For example, given a 2 x 3 matrix `Tensor` containing the following elements:

```
array([[ 1.0, 2.0, 3.0],
       [ 4.0, 5.0, 6.0]])
```

The image of the transpose `:::ada Tensor.Transpose` will print:

```
array([[ 1.0, 4.0],
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

### Outer product

The outer product of two vectors, 1-D tensors, returned by the function
`Outer`, is a matrix, a 2-D tensor. The number of rows is equal to the
size of the first vector and the number of columns is equal to the size
of the second vector.

!!! info "The difference between the outer and inner products"
    The outer product for two vectors **u** and **v** is defined as
    **u** **v**^T and is a *n* x *m* matrix, while the inner product
    (or dot product) is defined as **u**^T **v** is a 1 x 1 matrix.

### Solving A **x** = **b**

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

### Trace

The trace of a matrix or 2-D tensor is the sum of the elements on the main
diagonal and can be computed with the function `Trace`.
The optional parameter `Offset` can be given to specify the diagonal to use

### Main diagonal

Function `Main_Diagonal` returns a 1-D tensor filled with the elements on
the main diagonal of a 2-D tensor. The diagonal to use can be specified with
the optional parameter `Offset`.

### Decompositions

!!! bug "TODO"

## Creating tensors

Tensors can be created using other tensors by applying the various operations
mentioned above. If there is no previous tensor that can be used to create a
new tensor, a tensor can also be created from scratch using either an array
of elements or booleans, or by calling certain functions that create tensor
with certain values or properties:

- A tensor can be created and filled with a specific value, all zeros, or
  all ones.

- A tensor can be created with elements in a certain range or in a linear,
  logarithmic, or geometric space.

- An identity matrix or 2-D tensor with specific elements on the main
  diagonal can be created.

### From arrays

A 1-D tensor can be created from an array of elements using the function `To_Tensor`:

```ada
Tensor : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0));
```

A boolean tensor can be created from an array of booleans using the
function `To_Boolean_Tensor`:

```ada
Values : constant Boolean_Array := (False, True, False, True);
Tensor : constant CPU_Tensor := To_Boolean_Tensor (Values);
```

If a 2-D tensor is desired, provide the desired shape as the second parameter:

```ada
Matrix : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0), (2, 2));
```

!!! info
    The function `Reshape` can be used to create a 2-D tensor from a
    1-D tensor. This may require copying the data, which the use of a
    shape as the second parameter will avoid.

### Filled with some value

A tensor consisting solely of zeros (0.0) or ones (1.0) can be created with
the functions `Zeros` and `Ones`. Each function has one parameter, indicating
the number of elements for a 1-D tensor, or the shape of the returned
tensor.

For example, a 1-D tensor consisting of a 1000 ones can be created
by writing:

```ada
Tensor : constant CPU_Tensor := Ones (1_000);
```

While a zero matrix with the shape 4 x 8 is created with:

```ada
Tensor : constant CPU_Tensor := Zeros ((4, 8));
```

To create a tensor of a particular shape filled with a specific value, use
the function `Fill`. For example, a tensor with the shape `Shape` where
each element has the value *e*^`-Lambda` is created with:

```ada
E_Lambda : CPU_Tensor := Fill (Shape, Ada.Numerics.e ** (-Lambda));
```

### Range or space

The functions `Array_Range`, `Linear_Space`, `Log_Space`, and `Geometric_Space`
return a tensor containing numbers in the request space.

#### Range

Function `Array_Range` will return a tensor with numbers in the interval
from 0.0 (including) to (excluding) the given stop value, using a step size
of 1.0 between two adjacent elements.
For example, `:::ada Array_Range (3.0)` returns a tensor with the values
0.0, 1.0, and 2.0. Optionally, the start of the interval can be given by
calling the function with two or three parameters:

```ada
Tensor_1 : constant CPU_Tensor := Array_Range (2.0, 5.0);
Tensor_2 : constant CPU_Tensor := Array_Range (2.0, 5.0, Step => 1.0);
```

`Tensor_1` and `Tensor_2` both contain numbers in the interval [0.0, 5.0):
the numbers 2.0, 3.0, and 4.0. The start of the interval must be less than
the stop.
The third parameter `Step` is optional and has the default value 1.0. Its
value must be greater than 0.0 if given.

#### Linear space

Instead of specifying an interval and a step size, the number of elements
in the returned tensor can be given for the function `Linear_Space`:

```ada
Tensor_1 : constant CPU_Tensor := Linear_Space (1.0, 5.0, Count => 5);
```

This will create a tensor `Tensor_1` with the elements 1.0, 2.0, 3.0, 4.0, and 5.0.
Function `Linear_Space` has a fourth parameter `Interval` with the default
value `Closed`, which causes the function to return a tensor containing
numbers in a linear scale in the interval [start, stop].
If the value `Half_Open` is used instead, the interval will be [start, stop):

```ada
Tensor_2 : constant CPU_Tensor :=
  Linear_Space (1.0, 5.0, Count => 5, Interval => Half_Open);
```

Due to the value `Half_Open`, tensor `Tensor_2` will contain the numbers
1.0, 1.8, 2.6, 3.4, and 4.2 instead.

Unlike `Array_Range`, the start of the interval may be greater or equal to
the stop of the interval. If parameter `Start` is greater than `Stop`, the
numbers in the tensor will be decreasing.
If `Start` is equal to `Stop`, the interval is degenerate and all numbers
are equal to `Start` and `Stop`.

#### Logarithmic space

The function `Log_Space` can be used to create a tensor with numbers in
a logarithmic scale in the interval [base^start, base^stop] when interval
is closed and [base^start, base^stop) when half open.
The base can be specified with the optional fourth parameter `Base`.
Its default value is 10.0.

For example, a tensor with the numbers 100.0, 1000.0, and 10000.0 can be
created using the default base 10.0:

```ada
Tensor : constant CPU_Tensor := Log_Space (2.0, 4.0, Count => 3);
```

And a tensor with the numbers 2.0, 4.0, and 8.0 can be created by
specifying a base 2.0:

```ada
Tensor : constant CPU_Tensor := Log_Space (1.0, 3.0, Count => 3, Base => 2.0);
```

#### Geometric space

The function `Geometric_Space` is similar to `Log_Space` with the difference
that the actual start and stop of the interval instead of the exponents
are specified.

For example, a tensor with three numbers in the half open interval [0.0, 1000.0)
can be created with:

```ada
Tensor : constant CPU_Tensor :=
  Geometric_Space (1.0, 1_000.0, Count => 3, Interval => Half_Open);
```

The tensor will contain the numbers 1.0, 10.0, and 100.0.

### Identity matrix or diagonal

To create a square identity matrix, call function `Identity` with
the size of the matrix (rows and column) as the first parameter.
A second optional parameter controls on which diagonal the ones
are placed. The default value of this parameter is 0, which places
the ones on the main diagonal.
For example, a 3 x 3 matrix with the ones on the diagonal one position
above the main diagonal can be created as follows:

```ada
Tensor : constant CPU_Tensor := Identity (3, Offset => 1);
```

Printing the image of this tensor will display:

```
array([[ 0.0, 1.0, 0.0],
       [ 0.0, 0.0, 1.0],
       [ 0.0, 0.0, 0.0]])
```

To create a non-square matrix, provide two separate parameters representing
the number of rows and columns to function `Identity`.

If one wishes to create an identity matrix with certain elements instead of
ones on the diagonal, use function `Diagonal` instead. The returned 2-D tensor
will always be a square matrix with the number of rows and columns equal to
the number of elements in the given array.

```ada
Tensor_1 : constant CPU_Tensor := Diagonal ((1.0, 2.0, 3.0));
```

Alternatively, the elements from a 1-D tensor instead of an array can be used:

```ada
Main_Diagonal : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0));
Tensor_2      : constant CPU_Tensor := Diagonal (Main_Diagonal);
```

In the examples above, `Tensor_1` is equal to `Tensor_2`.

Just like `Identity`, the function `Diagonal` accepts a second parameter
that specifies on which diagonal the elements must be placed.

## Changing the shape

Sometimes the shape of a tensor needs to be changed.
For example, `:::ada Tensor (Tensor mod 2.0 = 0.0)` returns a 1-D tensor.
The function `Reshape` can be used to create a new tensor that has the given
shape and the elements of the original tensor. The function can be given
either a shape or the number of elements:

```ada
Tensor_1 : constant CPU_Tensor := To_Tensor ((1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
Tensor_2 : constant CPU_Tensor := Tensor_1.Reshape ((2, 3));
```

The given shape or size must match the number of elements of the tensor.
Elements cannot be removed or added by providing a shape or size that is
smaller or larger than the current size of the tensor.

Calling `Reshape` with the current number of elements as the first parameter
will just flatten the tensor to a 1-D tensor. This is a common operation, and
you can call the function `Flatten` instead if you want, which will do the
same thing.

## Concatenating

Concatenating tensors is possible with the operator `&`. This will concatenate
two tensors in the first dimension, increasing the number of rows to the sum of
the rows of the two tensors. For multidimensional tensors, the size of the first
dimension (the number of rows) can be different, but the size of the other
dimensions must be equal.

The function `Concatenate` can be used to concatenate tensors in dimensions
other than the first dimension by specifying the parameter `Dimension`.
For example, given the following two tensors:

```ada
Tensor_1 : constant CPU_Tensor := Diagonal ((1.0, 2.0, 3.0));
Tensor_2 : constant CPU_Tensor := To_Tensor ((4.0, 5.0, 6.0, 7.0, 8.0, 9.0), (3, 2));
```

The tensors can be concatenated horizontally with:

```ada
Tensor_3 : constant CPU_Tensor := Tensor_1.Concatenate (Tensor_2, Dimension => 2);
```

The image of `Tensor_3` will be:

```
array([[ 1.0, 0.0, 0.0, 4.0, 5.0],
       [ 0.0, 2.0, 0.0, 6.0, 7.0]
       [ 0.0, 0.0, 3.0, 8.0, 9.0]])
```

## Statistics

The minimum and maximum element in a tensor can be retrieved with the
functions `Min` and `Max`.

The function `Quantile` returns the element when the cumulative distribution
function of the tensor reaches the given probability `P`.
`Median` returns the middle element (when sorted), which is equal to
calling `Quantile` with `:::ada P = 0.5`.

The mean and variance can be computed using the functions `Mean` and `Variance`.
An optional parameter `Offset` can be used to return the unbiased sample variance
instead. The returned value is unbiased if `:::ada Offset = 1` and biased
if `:::ada Offset = 0`.

The standard deviation (the square root of the variance) is returned by
`Standard_Deviation`. Like `Variance`, it has an optional parameter `Offset`.
The returned value is always biased because of the square root, even
when `:::ada Offset = 1`.

### Statistical distributions

The generic package `Generic_Random` provides functions that return a
tensor with one of the following statistical distributions:

- `Uniform`. Values are uniformly distributed in the range 0 .. 1.

- `Normal`. Values are from the standard normal distribution with
  mean 0.0 and variance 1.0. To create a tensor with the distribution
  N(3.0, 2.0) (mean is 3.0 and standard deviation is 2.0), use:

   ```ada
   3.0 + Normal (Shape) * 2.0
   ```

- `Binomial` with parameters `N` and `P`. Returns a tensor where each
  element is the number of successful runs (each value is in 0 .. `N`)
  with each run having a probability of success `P`. Parameter `P`
  must be in 0.0 .. 1.0.

  For example, if we want to perform some experiment 10 times with a
  success probability of 0.1 (10 %) each, and want to know the probability
  that all 10 experiments fail, create a large tensor with a binomial
  distribution:

  ```ada
  Trials : constant := 20_000;

  Tensor : constant CPU_Tensor := Random.Binomial ((1 => Trials), N => 10, P => 0.1);
  Result : constant Element    := CPU_Tensor'(1.0 and (Tensor = 0.0)).Sum / Element (Trials);
  ```

  This gives a `Result` of roughly 0.35 or 35 %.

   !!! warning "Keep parameter `N` small for large tensors"
       The runtime cost of the implementation of `Binomial` might depend on
       `N`, thus this number should not be too large for very large tensors.

- `Geometric` with parameter `P`. Create a tensor with a geometric
  distribution, modeling the number of failures. Parameter `P` must be in
  0.0 .. 1.0.

- `Exponential` with parameter `Lambda`.

- `Pareto` with parameters `Xm` and `Alpha`.

- `Laplace` with parameters `Mean` and `B`.

- `Rayleigh` with parameter `Sigma`.

- `Weibull` with parameters `K` and `Lambda`.

- `Poisson` with parameter `Lambda`.

- `Gamma` with parameters `K` and `Theta`.

- `Beta` with parameters `Alpha` and `Beta`.

The package can be instantiated using a type derived from type `Tensor`,
for example:

```ada
use Orka.Numerics.Singles.Tensors;
use Orka.Numerics.Singles.Tensors.CPU;

package Random is new Generic_Random (CPU_Tensor);
```

The type `CPU_Tensor` in package `SIMD_CPU` uses the [xoshiro128++][url-xoshiro]
pseudo-random number generator and needs to be seeded once with a
Duration value before using any of the functions in the generic package:

```ada
Reset_Random (Orka.OS.Monotonic_Clock);
```

  [url-xoshiro]: https://prng.di.unimi.it/
