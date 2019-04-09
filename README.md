<h1 align="center">Orka</h1>

<h4 align="center">The OpenGL 4.6 Rendering Kernel in Ada 2012</h4>

<div align="center">

[![License](https://img.shields.io/github/license/onox/orka.svg?color=blue)](https://github.com/onox/orka/blob/master/LICENSE)
[![Build status](https://img.shields.io/shippable/5c87f0065329800700799d31/master.svg)](https://app.shippable.com/github/onox/orka)
[![GitHub release](https://img.shields.io/github/release/onox/orka.svg)](https://github.com/onox/orka/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20freenode-orange.svg)](https://webchat.freenode.net/?channels=ada)

</div>

Orka is an OpenGL 4.6 rendering kernel written in Ada 2012. It provides
an object-oriented API for modern OpenGL and implements various [AZDO][url-azdo]
techniques to allow multithreaded buffer updates and batched draw calls
for high performance rendering.

- **Object-oriented API**. Renderer objects like shader programs, vertex
formats, (mapped) buffers, and framebuffers, can be operated via an
object-oriented API that is easy to use. Any arithmetic that you might
need to perform when writing or reading data from buffers or other objects
is done for you automatically.

- **Job graph processing system**. A job graph processing system provides
flexible multitasking by allowing work to be split into multiple small jobs
which are then processed by any available task from a task pool. Jobs can be
processed in parallel as well as sequentially.

- **Asynchronous resource loading**. Resources like [KTX][url-ktx] textures
and [glTF 2.0][url-gltf] models are loaded asynchronously using the job graph
processing system.

- **Quaternions and matrices**. Packages for applying common transformations
to vectors, quaternions, and matrices using x86 SIMD instructions are provided.
The various x86 SIMD extensions like SSE, SSE2, SSE3, SSE4.1, AVX, and F16C
can also be used directly in your own code.

- **OpenGL 4.6 bindings**. Thick bindings are provided for the modern parts
of OpenGL 4.6. No bindings are provided for deprecated fixed function
functionality or functions that have been superseded by newer extensions.

- **GLFW 3 bindings**. Provides bindings for [GLFW 3.x][url-glfw]. This
is a library for creating windows with an OpenGL context on them. It also
provides functionality for capturing user input on keyboard, mouse, and
joystick. Having a window with an OpenGL context is the prerequisite for
using any OpenGL functionality. Alternatively, libraries like SDL can be
used instead to create windows and process input.

- **SDL 2 support**. Windows and input can be managed via [SDL][url-sdl]
instead of GLFW if desired.

## 🏗 Build status

Orka is supported on Linux and Windows. Support for macOS has been removed
due to its very outdated OpenGL drivers (most of the required OpenGL 4.x
extensions have not been implemented in their drivers).

|                    | Linux   | Windows     |
|--------------------|---------|-------------|
| **GNAT CE 2018**   |         | passing     |
| **GNAT FSF 7.3**   | passing |             |
| **GNAT FSF 8.2**   | passing |             |

## 📦 Dependencies

In order to build and use Orka you need to have:

 * An Ada 2012 compiler (Either [GNAT FSF][url-fsf] from
   your Linux distribution or [GNAT CE][url-ce])

 * GPRBuild and `make`

 * [json-ada][url-json-ada]

 * OpenGL 4.0 core profile and the following extensions:

    | Extension                            | OpenGL | Reason      |
    |--------------------------------------|--------|-------------|
    | ARB\_shader\_draw\_parameters        | 4.6    | glTF        |
    | ARB\_direct\_state\_access           | 4.5    |             |
    | ARB\_clip\_control                   | 4.5    | Reversed Z  |
    | ARB\_get\_texture\_sub\_image        | 4.5    | KTX writer  |
    | ARB\_buffer\_storage                 | 4.4    |             |
    | KHR\_debug                           | 4.3    | Debugging   |
    | ARB\_compute\_shader                 | 4.3    | Culling     |
    | ARB\_multi\_draw\_indirect           | 4.3    | glTF        |
    | ARB\_shader\_storage\_buffer\_object | 4.3    | glTF        |
    | ARB\_program\_interface\_query       | 4.3    | Subroutines |
    | ARB\_internalformat\_query2          | 4.3    | KTX writer  |
    | ARB\_vertex\_attrib\_binding         | 4.3    |             |
    | ARB\_texture\_storage\_multisample   | 4.3    | Textures    |
    | ARB\_texture\_storage                | 4.2    | Textures    |
    | ARB\_separate\_shader\_objects       | 4.1    |             |

 * An x86-64 CPU with the AVX and F16C extensions

Recommended dependencies:

 * [GLFW 3][url-glfw] for the GLFW bindings, tools, and examples

 * [SDLAda][url-sdlada] for managing windows and input via SDL 2

Optional dependencies:

 * [Ahven 2][url-ahven] if you want to build and run the unit tests

 * `lcov` to generate a coverage report for the unit tests

## Installing dependencies on Ubuntu 18.04 LTS

Install the dependencies using apt:

```sh
$ sudo apt install gnat-7 gprbuild make libahven6-dev lcov
$ sudo apt install libglfw3 libglfw3-dev libegl1-mesa-dev
```

Compile and install [json-ada][url-json-ada].

## Installing dependencies on Arch Linux

Install the dependencies using pacman and makepkg:

```sh
$ sudo pacman -S gcc-ada glfw-x11
```

You also need to compile and install GPRBuild:

```sh
$ sudo pacman -S --needed base-devel git
$ git clone https://aur.archlinux.org/gprbuild-bootstrap.git
$ git clone https://aur.archlinux.org/xmlada.git
$ git clone https://aur.archlinux.org/libgpr.git
$ git clone https://aur.archlinux.org/gprbuild.git
```

Go to each folder (`gprbuild-bootstrap`, then `xmlada`, `libgpr`, and `gprbuild`),
inspect the content of the files and then execute `makepkg -si`.

Compile and install [json-ada][url-json-ada].

## ⚙ Installation

A Makefile is provided to build the source code, examples, and tools.
Use `make` to build the source code:

```sh
$ make
```

You can override CFLAGS if desired. The Makefile determines which
system-dependent API ([EGL][url-egl] or WGL) to use for fetching OpenGL
function pointers.

To disable assertions and avoid checking after each call to OpenGL whether
an error flag was set and raise a corresponding exception, use the `release` mode:

```sh
$ make MODE=release
```

The default mode is `development`. Both `release` and `development` enable general
optimizations. To enable OpenGL exceptions, disable optimizations, and include
debugging symbols, use the `debug` mode. See the following table:

|                   | Release | Development | Debug |
|-------------------|---------|-------------|-------|
| Optimizations     | Yes     | Yes         | No    |
| Assertions        | No      | Yes         | Yes   |
| OpenGL exceptions | No      | Yes         | Yes   |
| Debugging symbols | No      | No          | Yes   |

After having compiled the source code, the library can be installed by
executing:

```sh
$ make PREFIX=/usr install
```

Change `PREFIX` to the preferred destination folder. Import `orka-glfw` (or
`orka-sdl`) in your \*.gpr project file:

```ada
with "orka-glfw";
```

## 🛠 Tools

The project provides tools to view glTF models, KTX textures, and to
display the OpenGL version and list the available extensions.

```sh
$ make tools
```

You can execute them in the `bin` directory. Some tools load shader
files from the source directory by using relative paths, so they only work
with `bin` as the current directory.

## Examples

The project contains some examples that demonstrate the basic usage of
the library. Build the example programs as follows:

```sh
$ make examples
```

You can execute them in the `bin` directory. Some examples load shader
files from the source directory by using relative paths, so they only work
with `bin` as the current directory.

## 🔬 Tests

The project contains a set of unit tests. Use `make test` to build and
run the unit tests. A coverage report can be generated with `make coverage`:

```sh
$ make test
$ make coverage
```

## 👏 Contributing

Read the [contributing guidelines][url-contributing] if you want to add
a bugfix or an improvement.

## 📄 License

Orka is distributed under the terms of the [Apache License 2.0][url-apache].

  [url-json-ada]: https://github.com/onox/json-ada
  [url-glfw]: http://www.glfw.org/
  [url-sdl]: http://www.libsdl.org/
  [url-sdlada]: https://github.com/Lucretia/sdlada
  [url-ce]: http://libre.adacore.com/
  [url-fsf]: https://gcc.gnu.org/wiki/GNAT
  [url-ahven]: http://ahven.stronglytyped.org
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-gltf]: https://github.com/KhronosGroup/glTF/blob/master/specification/2.0/README.md
  [url-ktx]: https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
  [url-egl]: https://www.khronos.org/egl
  [url-azdo]: https://www.khronos.org/assets/uploads/developers/library/2014-gdc/Khronos-OpenGL-Efficiency-GDC-Mar14.pdf
  [url-contributing]: /CONTRIBUTING.md
