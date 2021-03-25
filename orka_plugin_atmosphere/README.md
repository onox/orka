[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/orka_plugin_atmosphere.json)](https://alire.ada.dev/crates/orka_plugin_atmosphere.html)
![License](https://img.shields.io/badge/license-Apache--2.0%20AND%20BSD--3--Clause-blue)

# orka_plugin_atmosphere

This crate provides packages to render a realistic atmosphere using
precomputed atmospheric scattering.

A port of the [C++ implementation][url-paper-impl] of the paper [1] to Ada.

## Bibliography

[1] "Precomputed Atmospheric Scattering", Bruneton E., Neyret F.,
    Euro Graphics Symposium on Rendering, 2008, Vol 27 (4)

## License

This crate is distributed under the terms of the [Apache License 2.0][url-apache]
and the [BSD 3-Clause license][url-bsd-3].

The following packages and/or files are licensed under the
[BSD 3-Clause license][url-bsd-3]:

- Package `Orka.Features.Atmosphere` and child packages `Constants` and `Earth`

- Files in `data/shaders/atmosphere/`

  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-bsd-3]: https://opensource.org/licenses/BSD-3-Clause
  [url-paper-impl]: https://github.com/ebruneton/precomputed_atmospheric_scattering
