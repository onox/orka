[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/orka_numerics.json)](https://alire.ada.dev/crates/orka_numerics.html)
[![License](https://img.shields.io/github/license/onox/orka.svg?color=blue)](https://github.com/onox/orka/blob/master/LICENSE)

# orka_numerics

This crate provides:

- Runge-Kutta 4th order numerical integrators.

- Tensors (1-D and 2-D) using SIMD instructions provided by the orka_simd crate.
  Tensors provide the following features:

  * Constant indexing using a number, range, or another (boolean) tensor
  * Matrix multiplication and power
  * Inverse, transpose, outer product
  * Constructors to create tensors from arrays, filled w/ zeros or ones
  * Constructors to create a range of numbers, or linear/log/geom space
  * Constructors to create an identity matrix, or with a diagonal
  * Reshape, flatten, or concatenate tensors
  * Element-wise operations (arithmetic, rounding, trigonometry, compare)
  * Reductions using arbitrary expressions
  * Statistics (operations and generating statistical distributions)
  * Logical operations on boolean tensors

## License

This crate is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-apache]: https://opensource.org/licenses/Apache-2.0
