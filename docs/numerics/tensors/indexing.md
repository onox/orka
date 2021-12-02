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
Some_Element : constant Element := Tensor ((I, J));
```

## Multiple rows and columns

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

## Using a boolean tensor

Another way to retrieve elements of a tensor, is to use another (boolean)
tensor as the index. Each element in the tensor for which the boolean tensor
is `True` is stored in the returned tensor. The returned tensor is always 1-D,
no matter the number of dimensions of the original tensor.

For example, given a 2 x 3 matrix `Tensor` containing the following elements:

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
