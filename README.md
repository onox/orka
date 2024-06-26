<div align="center">
    <h1>Orka</h1>
    <a href="https://github.com/onox/orka/actions/workflows/build.yaml"><img src="https://github.com/onox/orka/actions/workflows/build.yaml/badge.svg" alt="Build status" /></a>
    <a href="https://github.com/onox/orka/actions/workflows/test.yml"><img src="https://github.com/onox/orka/actions/workflows/test.yml/badge.svg" alt="Test status" /></a>
    <a href="https://app.netlify.com/sites/orka-engine/deploys"><img src="https://img.shields.io/netlify/4fa61148-e68f-41e6-b7fa-1785eaf4bcb5?label=docs" alt="Docs status" /></a>
    <a href="https://github.com/onox/orka/blob/master/LICENSE"><img src="https://img.shields.io/github/license/onox/orka.svg?color=blue" alt="License" /></a>
    <a href="https://github.com/onox/orka/releases/latest"><img src="https://img.shields.io/github/release/onox/orka.svg" alt="GitHub release" /></a>
    <a href="https://gitter.im/ada-lang/Lobby"><img src="https://badges.gitter.im/gitterHQ/gitter.svg" alt="Gitter chat" /></a>
</div>

<br />
<br />

Orka is an OpenGL 4.6 rendering kernel written in Ada 2022. It provides
the building blocks like a frame graph to render 3D graphics or to do general-purpose
computing on the GPU.
It also takes care of displaying a window and manage input devices like gamepads.

- **Frame graph**. Create a frame graph with render passes and textures to describe
how a frame should be rendered. Any color or depth texture can be rendered to the
window of the application or to a KTX file.
The pipeline state needed for each render pass is updated automatically.
Multiple small frame graphs can be connected to each other to build a larger frame graph
with a few lines of code.

- **Windows and input devices**. Use the built-in [window toolkit][url-awt]
to manage input devices like the pointer, keyboard, and gamepads, and windows
that can display 3D graphics.
It has a similar purpose as GLFW and SDL, but is fully written in Ada.

- **Gamepads**. Apply mappings from
[SDL gamecontroller database][url-sdl-gamecontroller-db],
play force-feedback effects,
get the estimated orientation and angular velocity of the motion sensor,
get the capacity and charging state of the battery,
change the color of the LED,
and detect chords (groups of buttons), button sequences,
and rapid button tapping.

- **Algorithms and effects**. Compute a prefix sum or a Fast Fourier Transform
using compute shaders, or apply a blurring effect to a texture.

- **Atmosphere and terrain**. Render a realistic atmosphere or adaptive
tessellated terrain of planets.

- **Debug rendering and logging**. Various packages exist which can be used
to draw bounding boxes, coordinate axes, lines, and spheres for debugging.
Messages from the rendering API or other parts of your application can be
logged to the terminal or files.

- **Transforms**. Apply common transformations to vectors, quaternions, and
matrices using x86 SIMD instructions.

- **Tensors and numerics**. Perform element-wise operations, reductions
using arbitrary expressions, or matrix operations on tensors using SIMD
instructions on the CPU or compute shaders on the GPU.
Generate tensors with some statistical distribution,
or use Runge-Kutta 4th order numerical integrators or sigma-point Kalman filters.

- **Surfaceless rendering**. Create a surfaceless rendering context without
any dependency on a windowing system for using compute shaders on a server.

- **Asynchronous resource loading**. Load resources like [KTX][url-ktx] textures
and [glTF][url-gltf] models asynchronously.
Resources can be loaded from directories and archive files.

Additionally, Orka provides several bindings:

- **x86 SIMD extensions**
Bindings for various x86 SIMD extensions, including SSE, SSE2, SSE3, SSSE3,
SSE4.1, AVX, AVX2, FMA, and F16C, and an implementation of the
[xoshiro][url-xoshiro] pseudo-random number generator using SSE2 or
AVX2 intrinsics.

- **OpenGL 4.6**. Thick bindings are provided for the modern parts
of OpenGL 4.6. There are no bindings for fixed function functionality
that is deprecated or functions that have been superseded by newer extensions.

- **EGL**. Thick bindings for EGL are provided to create a surfaceless
context for rendering without the presence of a windowing system.

## Documentation

The documentation can be viewed [on the website][url-docs].

## Learning Ada

Ada is an imperative and object-oriented programming language focused
on correctness, readability, and good [software engineering practices][url-swe-practices]
for large scale systems and safety-critical and embedded real-time systems.

It has a strong static type system which allows you to create your own
types that reflect the problem domain, with optional low-level control
of your data. Packages provide modularity and information hiding. High-level
concurrency primitives like protected objects allow safe communication
between tasks and design-by-contract is supported through type invariants,
predicates, and pre- and postconditions.

If you would like to learn Ada, then here are a few resources to get started:

- [Introduction to Ada][url-learn-act] (An interactive tutorial in your browser)

- [Ada Programming on Wikibooks][url-wikibooks]

- [(Online) books on Awesome Ada][url-awesome]

## Contributing

If you would like to fix a bug, add a feature, improve the documentation or
have suggestions or advice about the architecture, APIs, or performance,
then do not hesitate to open a new [issue][url-issue].

See the [contributing guidelines][url-contributing] for more information.

## License

Most Orka crates are distributed under the terms of the [Apache License 2.0][url-apache]
except for a few separate Alire crates:

- Crate [orka_plugin_atmosphere][url-crate-atmosphere] is licensed under
the [Apache License 2.0][url-apache] AND [BSD 3-Clause license][url-bsd-3].

- Crate [orka_plugin_terrain][url-crate-terrain] is licensed under
the [Apache License 2.0][url-apache] AND [MIT license][url-mit].

  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-awt]: https://github.com/onox/orka/tree/master/awt
  [url-bsd-3]: https://opensource.org/licenses/BSD-3-Clause
  [url-crate-atmosphere]: https://github.com/onox/orka/tree/master/orka_plugin_atmosphere
  [url-crate-terrain]: https://github.com/onox/orka/tree/master/orka_plugin_terrain
  [url-mit]: https://opensource.org/licenses/MIT
  [url-awesome]: https://github.com/ohenley/awesome-ada#online-books
  [url-azdo]: https://www.khronos.org/assets/uploads/developers/library/2014-gdc/Khronos-OpenGL-Efficiency-GDC-Mar14.pdf
  [url-contributing]: /CONTRIBUTING.md
  [url-docs]: https://orka-engine.netlify.com/
  [url-gltf]: https://github.com/KhronosGroup/glTF/blob/master/specification/2.0/README.md
  [url-issue]: https://github.com/onox/orka/issues
  [url-ktx]: https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
  [url-learn-act]: https://learn.adacore.com/courses/intro-to-ada/index.html
  [url-sdl-gamecontroller-db]: https://github.com/gabomdq/SDL_GameControllerDB
  [url-swe-practices]: https://en.wikibooks.org/wiki/Ada_Programming#Programming_in_the_large
  [url-wikibooks]: https://en.wikibooks.org/wiki/Ada_Programming
  [url-xoshiro]: https://prng.di.unimi.it/
