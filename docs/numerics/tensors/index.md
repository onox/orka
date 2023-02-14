# Tensors

Tensors are multidimensional arrays of numbers on which element-wise
operations such as arithmetic or trigonometry can be performed.
Some tensors, those which are 2-D, support special matrix operations
like matrix multiplication, computing the inverse or transpose, or
solving a set of linear equations.

Furthermore, tensors can be created with a specific statistical distribution
or reduced to a single number with basic arbitrary expressions
(for example, to compute the sum or product).

Tensors provide the following features:

- Constant [indexing](/numerics/tensors/indexing/) using a number, range,
  or another (boolean) tensor
- [Matrix operations](/numerics/tensors/matrix-operations/)
  (multiplication, power and inverse, transpose, outer, solving A **x** = **b**, least-squares)
- Matrix decompositions like QR, Cholesky
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

## Implementations

Two crates exist which implement the `Tensor` interface: one which uses
SIMD instructions on the CPU and one which uses buffers and compute shaders
on the GPU:

| Crate                                       | Implementation           | Evaluation | Tensors |
|---------------------------------------------|--------------------------|------------|---------|
| [orka\_tensors\_cpu][url-tensors-cpu-crate] | SIMD instructions on CPU | Eager      | Small   |
| [orka\_tensors\_gpu][url-tensors-gpu-crate] | Compute shaders on GPU   | Lazy       | Large   |

The SIMD implementation uses x86 SIMD instructions and has certain characteristics:

- Numbers in tensors are always floating-point numbers

- No pointers are used; functions always return a new tensor and
  do not modify the tensor parameters of a function. Thus operations
  are evaluated immediately and there's little room for additional
  optimizations besides the use of SIMD instructions.

The GPU implementation uses compute shaders and stores tensors in buffers
on the GPU. On an integrated GPU these buffers may be as small as 128 MiB,
but discrete GPUs may support larger buffers of up to 2 GiB.

Furthermore, the GPU implementation builds a directed acyclic graph of operations
and materializes the data only at the last possible moment,
such as when one or more elements are retrieved from the tensor with a getter
function or when you switch from a sequence of element-wise operations to
a matrix operation, for example.

??? bug "Limitations of tensors"
    All tensors have the following limitations:

    - Tensors of three dimensions or higher are partially supported.
      Element-wise operations are supported, but some matrix operations
      need to be modified to handle tensors with 3 or 4 axes.

    - Most functions operate on tensors containing floating-point numbers
      because of the generic parameter of the package `:::ada Orka.Numerics.Tensors`.
      Certain implementations may supports tensors containing boolean or
      (unsigned) integers.

## Dependencies

The SIMD implementation in the [orka\_tensors\_cpu][url-tensors-cpu-crate]
requires one of the following x86 extensions: SSE 4.1, AVX, or AVX2.

The GPU implementation in [orka\_tensors\_gpu][url-tensors-gpu-crate]
requires OpenGL extensions for SSBOs and compute shaders, plus a few others:

!!! summary "Required OpenGL extensions for the GPU implementation"

    | Extension                            | OpenGL |
    |--------------------------------------|--------|
    | ARB\_compute\_shader                 | 4.3    |
    | ARB\_compute\_variable\_group\_size  |        |
    | ARB\_shader\_storage\_buffer\_object | 4.3    |
    | ARB\_shader\_clock                   |        |

    Most GPUs from 2012 or later should have these extensions if you use
    a video driver provided by your Linux distribution.

  [url-tensors-cpu-crate]: https://github.com/onox/orka/tree/master/orka_tensors_cpu
  [url-tensors-gpu-crate]: https://github.com/onox/orka/tree/master/orka_tensors_gpu

*[SIMD]: Single Instruction Multiple Data
*[SSBO]: Shader Storage Buffer Object
