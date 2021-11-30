# Element-wise operations

All tensors, no matter which number of dimensions they have, support
the many element-wise operations in `:::ada Orka.Numerics.Tensors`,
using the operators provided by the Ada language.
Binary operators support operations on two tensors as well as one tensor
and one element.

## Arithmetic

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

## Rounding

To round numbers up, down, or the nearest integral value, the function
`Ceil`, `Floor`, or `Round` can be used. To truncate the floating-point
numbers, use the function `Truncate`.

## Math

The square-root can be used obtained with the function `Sqrt`.
The operation *e*^x^, where x are the elements of a tensor, can
be performed using the function `Exp`. The natural logarithm with
the function `Log`, and the base 10 and base 2 logarithms with `Log10`
and `Log2`.

A tensor containing elements that are the minimum or maximum of a
pair of elements can be created with the functions `Min` and `Max`.
These two functions can operate on two tensors or a tensor and a single
element.

## Trigonometry

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

## Logical operations

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

## Comparing

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
