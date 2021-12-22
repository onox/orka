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

- Constant [indexing](/numerics/tensors/indexing/) using a number, range,
  or another (boolean) tensor
- [Matrix operations](/numerics/tensors/matrix-operations/)
  (multiplication, power and inverse, transpose, outer, solving A **x** = **b**)
- Constructors to create tensors from arrays, filled with zeros or ones
- Constructors to create a range of numbers, or linear/log/geom space
- Constructors to create an identity matrix, or with a diagonal
- Reshape, flatten, or concatenate tensors
- [Element-wise operations](/numerics/tensors/element-wise-operations/)
  (arithmetic, rounding, trigonometry, compare)
- Logical operations on boolean tensors
- Reductions using arbitrary [expressions](/numerics/tensors/expressions/)
- [Statistics](/numerics/tensors/statistics/) (operations and generating
  statistical distributions)

??? bug "Limitations of tensors"
    All tensors have the following limitations:

    - Tensors of three dimensions or higher are currently not supported.
      Element-wise operations do not need modifications, but other functions
      may need to be modified to handle 3-D tensors.

    - Numbers in tensors are always floating-point numbers because of the
      generic parameter of the package `:::ada Orka.Numerics.Tensors`.

    - Special matrix decompositions are not implemented yet.

??? note "Variable indexing"
    Some implementations of tensors may (partially) support variable
    indexing to assign values to specific parts of a tensor.

##  Image

The image of a tensor can be obtained with the function `Image`:

```ada
Orka.OS.Put_Line (Tensor.Image);
```
