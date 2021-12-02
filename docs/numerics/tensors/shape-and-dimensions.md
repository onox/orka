# Shape and dimensions

Each tensor has a shape, the number of elements in each dimension,
of the type `Tensor_Shape`; an indefinite array of natural numbers.
For example, the shape of a 2 × 3 matrix is `:::ada (2, 3)` and the shape
of a vector of 100 elements is `:::ada (1 => 100)`.

The shape of a tensor can be retrieved using the function `Shape` and
the dimensions with the function `Dimensions`. The total number of
elements in the tensor is queried with the function `Elements`.

It is true that `:::ada T.Shape'Length = T.Dimensions` and
`:::ada Elements (T.Shape) = T.Elements` for a tensor `T`.

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
tensor([[ 1.0, 0.0, 0.0, 4.0, 5.0],
        [ 0.0, 2.0, 0.0, 6.0, 7.0]
        [ 0.0, 0.0, 3.0, 8.0, 9.0]])
```
