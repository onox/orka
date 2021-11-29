[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/orka_simd.json)](https://alire.ada.dev/crates/orka_simd.html)
[![License](https://img.shields.io/github/license/onox/orka.svg?color=blue)](https://github.com/onox/orka/blob/master/LICENSE)

# orka_simd

This crate provides bindings for the various x86 SIMD extensions, including
SSE, SSE2, SSE3, SSSE3, SSE4.1, AVX, AVX2, FMA, and F16C, and an implementation
of the [xoshiro128++][url-xoshiro] pseudo-random number generator using SSE2 or
AVX2 intrinsics.

## License

This crate is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-xoshiro]: https://prng.di.unimi.it/
