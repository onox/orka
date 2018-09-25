[![License](https://img.shields.io/:license-Apache_License_2.0-blue.svg)](https://github.com/onox/orka/blob/master/LICENSE.md)

Orka
====

Orka is the OpenGL 4.6 Rendering Kernel in Ada. It is written in Ada 2012
and provides an object-oriented API for modern OpenGL. Orka makes it easy
to construct OpenGL programs and meshes and to use them in a scene tree.
Orka and the OpenGL bindings require and use OpenGL 4.5's Direct State
Access (DSA) extension.

Orka builds upon and provides thick bindings for OpenGL 4.6. These bindings
are based on the original [OpenGLAda][url-openglada] bindings. Bindings for
the fixed function functionality have been removed and bindings for various
extensions of OpenGL 4.x have been added.

Additionally, it provides bindings for [GLFW 3.x][url-glfw]. This is a library
for creating windows with an OpenGL context on them. It also provides
functionality for capturing user input on keyboard, mouse and joystick.
Having a window with an OpenGL context is the prerequisite for using any
OpenGL functionality.

Orka is supported on Linux and Windows. Support for OS X has been removed
due to its very outdated OpenGL drivers (most of the required OpenGL 4.x
extensions have not been implemented in their drivers).

Features
--------

 * Thick OpenGL 4.6 bindings
 * Thick GLFW 3 bindings
 * Various x86 SIMD extensions like SSE, SSE2, SSE3, SSE4.1, AVX, and F16C
 * Transforms (vectors, quaternions, and matrices) and scene trees (uses the x86 SIMD extensions)
 * Easy construction of shader programs, buffers, framebuffers, and vertex formats
 * Job graph processing system for better utilization of all CPU cores
 * Game loop
 * Camera's
 * [glTF 2.0][url-gltf] loader (uses MDI)
 * [KTX][url-ktx] loader and writer

Build status
------------

|                    | Linux   | Windows     |
|--------------------|---------|-------------|
| **GNAT GPL 2017**  | failing | failing     |
| **GNAT CE 2018**   | unknown | unknown     |
| **GNAT FSF 7.3**   | passing |             |
| **GNAT FSF 8.2**   | passing |             |

Dependencies
------------

In order to build Orka you need to have:

 * A GNAT Ada 2012 compiler with GPRBuild (Either [GNAT FSF][url-fsf] from
   your Linux distribution or [GNAT CE][url-ce])

 * [json-ada][url-json-ada]

 * OpenGL 4.0 core profile and the following extensions:

    | Extension                            | OpenGL | Reason      |
    |--------------------------------------|--------|-------------|
    | ARB\_shader\_draw\_parameters        | 4.6    | glTF        |
    | ARB\_direct\_state\_access           | 4.5    |             |
    | ARB\_clip\_control                   | 4.5    | Reversed Z  |
    | ARB\_buffer\_storage                 | 4.4    |             |
    | KHR\_debug                           | 4.3    | Debugging   |
    | ARB\_multi\_draw\_indirect           | 4.3    | glTF        |
    | ARB\_shader\_storage\_buffer\_object | 4.3    | glTF        |
    | ARB\_program\_interface\_query       | 4.3    | Subroutines |
    | ARB\_vertex\_attrib\_binding         | 4.3    |             |
    | ARB\_texture\_storage\_multisample   | 4.3    | Textures    |
    | ARB\_texture\_storage                | 4.2    | Textures    |
    | ARB\_separate\_shader\_objects       | 4.1    |             |

 * An x86-64 CPU with the AVX and F16C extensions

Recommended dependencies:

 * [GLFW 3][url-glfw] for the GLFW bindings

Optional dependencies:

 * [Ahven 2][url-ahven] if you want to build and run the unit tests

Installing dependencies on Ubuntu 18.04 LTS
-------------------------------------------

Install the dependencies using apt:

```sh
$ sudo apt install gnat-7 gprbuild libahven6-dev
$ sudo apt install libglfw3 libglfw3-dev libegl1-mesa-dev
```

Compile and install [json-ada][url-json-ada].

Installing dependencies on Arch Linux
-------------------------------------

Install the dependencies using pacman and makepkg:

```sh
$ sudo pacman -S gcc-ada glfw-x11
```

You also need to compile and install GPRBuild:

```sh
$ sudo pacman -S --needed base-devel
$ git clone https://aur.archlinux.org/gprbuild-bootstrap.git
$ git clone https://aur.archlinux.org/xmlada.git
$ git clone https://aur.archlinux.org/gprbuild.git
```

Go to each folder (`gprbuild-bootstrap`, then `xmlada`, then `gprbuild`),
inspect the content of the files and then execute `makepkg -si`.

Compile and install [json-ada][url-json-ada].

Compilation
-----------

A Makefile is provided to build the source code, examples, and tools.
Use `make` to build the source code:

```sh
$ MODE=release make
```

You can override CFLAGS if desired. The Makefile determines which
system-dependent API ([EGL][url-egl], GLX, or WGL) to use for fetching OpenGL
function pointers. Adjust the Makefile if necessary.

If you want to check after each call to OpenGL whether an error flag was set
and raise a corresponding exception, then use the `development` mode:

```sh
$ MODE=development make
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

Tools
-----

The project provides tools to view glTF models, KTX textures, and to
display the OpenGL version and list the available extensions.

```sh
$ make tools
```

You can execute them in the `bin` directory. Some tools load shader
files from the source directory by using relative paths, so they only work
with `bin` as the current directory.

Examples
--------

The project contains some examples that demonstrate the basic usage of
the library. Build the example programs as follows:

```sh
$ make examples
```

You can execute them in the `bin` directory. Some examples load shader
files from the source directory by using relative paths, so they only work
with `bin` as the current directory.

Tests
-----

The project contains a set of unit tests. Use `make test` to build the unit tests:

```sh
$ make test
```

After having build the tests, run the unit tests:

```sh
$ make run_unit_tests
```

Installation
------------

After having compiled the source code, the library can be installed by executing:

```sh
$ make PREFIX=/usr install
```

Change `PREFIX` to the preferred destination folder.

Using Orka in your project
--------------------------

Specify the dependency in your \*.gpr project file:

```ada
with "orka-glfw";
```

If you do not want to use GLFW, refer to `orka` instead.

License
-------

The OpenGL and GLFW bindings and Orka are distributed under the terms
of the [Apache License 2.0][url-apache].

  [url-openglada]: https://github.com/flyx/OpenGLAda
  [url-json-ada]: https://github.com/onox/json-ada
  [url-glfw]: http://www.glfw.org/
  [url-ce]: http://libre.adacore.com/
  [url-fsf]: https://gcc.gnu.org/wiki/GNAT
  [url-ahven]: http://ahven.stronglytyped.org
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-gltf]: https://github.com/KhronosGroup/glTF/blob/master/specification/2.0/README.md
  [url-ktx]: https://www.khronos.org/opengles/sdk/tools/KTX/file_format_spec/
  [url-egl]: https://www.khronos.org/egl

