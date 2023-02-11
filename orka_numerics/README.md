[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/orka_numerics.json)](https://alire.ada.dev/crates/orka_numerics.html)
[![License](https://img.shields.io/github/license/onox/orka.svg?color=blue)](https://github.com/onox/orka/blob/master/LICENSE)

# orka_numerics

This crate provides:

- Runge-Kutta 4th order [numerical integrators][url-docs-integrators].

- Sigma-point [Kalman filters][url-docs-kalman] (UKF and SR-CDKF) for
  state and parameter estimation.

- [Tensor][url-docs-tensors] interface with the following features:

  * Constant indexing using a number, range, or another (boolean) tensor
  * Matrix multiplication, power, inverse, transpose, outer product
  * Matrix decompositions like QR, Cholesky
  * Solve equations like Ax = b or find (constrained) least-squares solutions
  * Vector normalization and standardization
  * Constructors to create tensors from arrays, filled w/ zeros or ones
  * Constructors to create a range of numbers, or linear/log/geom space
  * Constructors to create an identity matrix, or with a diagonal
  * Reshape, flatten, or concatenate tensors
  * Element-wise operations (arithmetic, rounding, trigonometry, compare)
  * Reductions using arbitrary expressions
  * Statistics (operations and generating statistical distributions)
  * Logical operations on boolean tensors

Two crates exist which implement the `Tensor` interface: one which uses
SIMD instructions on the CPU and one which uses buffers and compute shaders
on the GPU.

| Crate                                     | Implementation           | Evaluation | Tensors |
|-------------------------------------------|--------------------------|------------|---------|
| [orka_tensors_cpu][url-tensors-cpu-crate] | SIMD instructions on CPU | Eager      | Small   |
| [orka_tensors_gpu][url-tensors-gpu-crate] | Compute shaders on GPU   | Lazy       | Large   |

## License

This crate is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-tensors-cpu-crate]: https://github.com/onox/orka/tree/master/orka_tensors_cpu
  [url-tensors-gpu-crate]: https://github.com/onox/orka/tree/master/orka_tensors_gpu
