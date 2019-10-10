[![License](https://img.shields.io/github/license/onox/orka.svg?color=blue)](https://github.com/onox/orka/blob/master/LICENSE)
[![Build status](https://img.shields.io/shippable/5c87f0065329800700799d31/master.svg)](https://app.shippable.com/github/onox/orka)
[![GitHub release](https://img.shields.io/github/release/onox/orka.svg)](https://github.com/onox/orka/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20freenode-orange.svg)](https://webchat.freenode.net/?channels=ada)

# Orka

Orka is an OpenGL 4.6 rendering kernel written in Ada 2012. It provides
an object-oriented API for modern OpenGL and implements various [AZDO][url-azdo]
techniques to allow multithreaded buffer updates and batched draw calls
for high performance rendering.

- **Object-oriented API**. Renderer objects like shader programs, vertex
formats, buffers, and framebuffers, can be operated via an object-oriented
API that is easy to use. Objects are automatically created and destroyed
by using controlled types.

- **Job graph processing system**. A job graph processing system provides
flexible multitasking by allowing work to be split into multiple small jobs
which are then processed by any available task from a task pool. Jobs can be
processed in parallel as well as sequentially.

- **Asynchronous resource loading**. Resources like [KTX][url-ktx] textures
and [glTF][url-gltf] models are loaded asynchronously using the job graph
processing system.

- **Quaternions and matrices**. Packages for applying common transformations
to vectors, quaternions, and matrices using x86 SIMD instructions are provided.
The various x86 SIMD extensions like SSE, SSE2, SSE3, SSE4.1, AVX, and F16C
can also be used directly in your own code.

Additionally, Orka provides several bindings:

- **OpenGL 4.6**. Thick bindings are provided for the modern parts
of OpenGL 4.6. No bindings are provided for deprecated fixed function
functionality or functions that have been superseded by newer extensions.

- **GLFW 3**. Provides bindings for [GLFW 3.x][url-glfw]. This
is a library for creating windows with an OpenGL context on them. It also
provides functionality for capturing user input on keyboard, mouse, and
joystick. Having a window with an OpenGL context is the prerequisite for
using any OpenGL functionality. Alternatively, libraries like [SDL][url-sdl] can be
used instead to create windows and process input.

## Learning Ada

Ada is an imperative and object-oriented programming language focused
on correctness, readability, and good [software engineering practices][url-swe-practices]
for large scale systems and safety-critical and embedded real-time systems.

It has a very strong static type system where you can create your own
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

If you would like to fix a bug, add an improvement, or have suggestions
or advice regarding the architecture, APIs, performance, or code comments
and documentation, then do not hesitate to open a new [issue][url-issue].

Make sure you have read the [contributing guidelines][url-contributing]
before opening an issue or pull request.

## License

Orka is distributed under the terms of the [Apache License 2.0][url-apache].
The first line of each Ada file should contain an SPDX license identifier
tag that refers to this license:

    SPDX-License-Identifier: Apache-2.0

  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-awesome]: https://github.com/ohenley/awesome-ada#online-books
  [url-azdo]: https://www.khronos.org/assets/uploads/developers/library/2014-gdc/Khronos-OpenGL-Efficiency-GDC-Mar14.pdf
  [url-contributing]: /contributing
  [url-glfw]: http://www.glfw.org/
  [url-gltf]: https://github.com/KhronosGroup/glTF/blob/master/specification/2.0/README.md
  [url-issue]: https://github.com/onox/orka/issues
  [url-ktx]: https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
  [url-learn-act]: https://learn.adacore.com/courses/intro-to-ada/index.html
  [url-sdl]: http://www.libsdl.org/
  [url-swe-practices]: https://en.wikibooks.org/wiki/Ada_Programming#Programming_in_the_large
  [url-wikibooks]: https://en.wikibooks.org/wiki/Ada_Programming
