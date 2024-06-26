# Indexing

To get elements at a certain index, tensors support Ada 2012's indexing
syntax. Several use cases are supported:

- Retrieving a single element of a 1-D or 2-D tensor, or a single row of
  a 2-D tensor.

- Retrieving a range of elements (1-D) or a range of rows (2-D).

- Retrieving a slice of elements consisting of multiple rows and multiple columns.

- Retrieving a number of elements selected using a boolean tensor.

## A row or value

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
Some_Element : constant Element := Tensor ([I, J]);
```

## Multiple rows and columns

If the given index is a `Range_Type` then the elements or rows at the given
range is returned depending on whether the tensor is 1-D or 2-D:

```ada
Some_Rows : constant CPU_Tensor := Tensor (Range_Type'(Start => 5, Stop => 10));
```

If one wants to extract multiple rows and/or multiple columns, an index of
the type `Tensor_Range` can be used. For example, given a 4 × 4 matrix,
the last two rows and the last three columns can be retrieved as follows:

```ada
Sub_Matrix : constant CPU_Tensor := Tensor (Tensor_Range'((3, 4), (2, 4)));
```

## Using a boolean tensor

Another way to retrieve elements of a tensor, is to use another (boolean)
tensor as the index. Each element in the tensor for which the boolean tensor
is `True` is stored in the returned tensor. The returned tensor is always 1-D,
no matter the number of dimensions of the original tensor.

For example, given a 2 × 3 matrix `Tensor` containing the following elements:

```
tensor([[ 1.0, 2.0, 3.0],
        [ 4.0, 5.0, 6.0]])
```

Then `:::ada Tensor (Tensor > 4.0).Image` prints the following:

```
tensor([ 5.0, 6.0])
```

And `:::ada Tensor (Tensor mod 2.0 = 0.0).Image` will print:

```
tensor([ 2.0, 4.0, 6.0])
```

See [Comparing](/numerics/tensors/element-wise-operations/#comparing) for
more information about how to create a boolean tensor.

## Assigning values

A single element or `Boolean` can be assigned using procedure `Set`.
The index must be of type `Tensor_Index`, which is an array.
For example, to assign the value `:::ada 4.0` to the second position
of a vector, the procedure `Set` can be used as follows:

```ada
Tensor.Set ([2], 4.0);
```

Similarly, the values `True` and `False` can be assigned to boolean
tensors.

### Tensors

Besides scalar values, it is also possible to assign a whole tensor.
The tensor must be of the same implementation (type) and the index
can be one of the following types:

- `Index_Type`

- `Range_Type`

- `Tensor_Range`

When assigning a tensor, the shape of the tensor must match the shape
of the part selected by the given index.

For example, if a matrix `Tensor` has shape 3 × 2 and `:::ada 2` is used
as the index, then the tensor given as the value must be a 1-D tensor with
2 elements (the number of columns of `Tensor`):

```ada
declare
   Tensor : CPU_Tensor := To_Tensor ([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], Shape => [3, 2]);
begin
   Tensor.Set (2, To_Tensor ([7.0, 8.0]));
end;
```

After assignment the values of `Tensor` are:

```
tensor([[ 1.0, 2.0],
        [ 7.0, 8.0],
        [ 5.0, 6.0]])
```

Similarly, a `Range_Type` or `Tensor_Range` can be used:

```ada
Tensor_1.Set (Range_Type'(2, 3), Tensor_2);
```

The index of type `Range_Type` will select a range of rows (in case of
a 2-D tensor).
If `Tensor_1` has shape 4 × 4, then `Tensor_2` must have shape 2 × 4.
The rows of `Tensor_2` must match the number of selected rows, and
the columns of the tensor must match the number of columns of `Tensor_1`.
