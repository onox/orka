# Creating tensors

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

## From arrays

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

## Filled with some value

A tensor consisting solely of zeros (0.0) or ones (1.0) can be created with
the functions `Zeros` and `Ones`. Each function has one parameter, indicating
the number of elements for a 1-D tensor, or the shape of the returned
tensor.

For example, a 1-D tensor consisting of a 1000 ones can be created
by writing:

```ada
Tensor : constant CPU_Tensor := Ones (1_000);
```

While a zero matrix with the shape 4 × 8 is created with:

```ada
Tensor : constant CPU_Tensor := Zeros ((4, 8));
```

To create a tensor of a particular shape filled with a specific value, use
the function `Fill`. For example, a tensor of some shape where
each element has the value *e*^-λ^ is created with:

```ada
E_Lambda : CPU_Tensor := Fill (Shape, Ada.Numerics.e ** (-Lambda));
```

## Range or space

The functions `Array_Range`, `Linear_Space`, `Log_Space`, and `Geometric_Space`
return a tensor containing numbers in the request space.

### Range

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

### Linear space

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

### Logarithmic space

The function `Log_Space` can be used to create a tensor with numbers in
a logarithmic scale in the interval [base^start^, base^stop^] when interval
is closed and [base^start^, base^stop^) when half open.
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

### Geometric space

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

## Identity matrix or diagonal

To create a square identity matrix, call function `Identity` with
the size of the matrix (rows and column) as the first parameter.
A second optional parameter controls on which diagonal the ones
are placed. The default value of this parameter is 0, which places
the ones on the main diagonal.
For example, a 3 × 3 matrix with the ones on the diagonal one position
above the main diagonal can be created as follows:

```ada
Tensor : constant CPU_Tensor := Identity (3, Offset => 1);
```

Printing the image of this tensor will display:

```
tensor([[ 0.0, 1.0, 0.0],
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
