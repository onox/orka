[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/orka_plugin_terrain.json)](https://alire.ada.dev/crates/orka_plugin_terrain.html)
![License](https://img.shields.io/badge/license-Apache--2.0%20AND%20MIT-blue)

# orka_plugin_terrain

This crate provides packages to render adaptively tessellated terrain on
the GPU based on the [LEB library][url-leb].

- Updates and renders multiple terrain tiles and uses heuristics to
determine on the CPU which tiles need to be updated and rendered.

- Supports flattened spheroids with warping to reduce RMSE when
projecting cubes on spheres.

- Can optionally display a wireframe on top of the rendered terrain.

## License

This crate is distributed under the terms of the [Apache License 2.0][url-apache]
and the [MIT license][url-mit].

The GLSL shaders of the LEB library and the shaders for rendering the terrain,
both in `data/shaders/terrain/`, are licensed under the MIT license.

  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-mit]: https://opensource.org/licenses/MIT
  [url-leb]: https://github.com/jdupuy/LongestEdgeBisection
