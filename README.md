[![License](https://img.shields.io/:license-Apache_License_2.0-blue.svg)](https://github.com/onox/orka/blob/master/LICENSE.md)

Orka
====

Orka is the OpenGL Rendering Kernel in Ada. It is written in Ada 2012
and provides an object-oriented API for modern OpenGL. Orka makes it easy
to construct OpenGL programs and meshes and to use them in a scene tree.
Orka and the OpenGL bindings require and use OpenGL 4.5's Direct State
Access (DSA) extension.

Orka builds upon and provides thick bindings for OpenGL 4.5. These bindings
are based on the original [OpenGLAda][url-openglada] bindings. Bindings for
the fixed function functionality have been removed and bindings for various
extensions of OpenGL 4.x have been added.

Additionally, it provides bindings for [GLFW 3.x][url-glfw]. This is a library
for creating windows with an OpenGL context on them. It also provides
functionality for capturing user input on keyboard, mouse and joystick.
Having a window with an OpenGL context is the prerequisite for using any
OpenGL functionality.

Features
--------

 * Thick OpenGL 4.5 bindings
 * Thick GLFW 3 bindings
 * Various x86 SIMD extensions like SSE, SSE2, SSE3, SSE4.1 and AVX
 * Easy construction of OpenGL programs and meshes
 * Transforms and scene tree (makes use of the x86 SIMD extensions)
 * glTF loader (uses MDI)

Dependencies
------------

In order to build Orka or the OpenGL bindings, you need to have:

 * A GNAT compiler that supports Ada 2012 (Either GNAT GPL from [AdaCore's Libre Site][url-adacore],
   or the GNAT [version provided by the FSF with GCC][url-fsf])

 * [GPRBuild][url-gprbuild] (Is bundled with AdaCore's GNAT distribution)

 * OpenGL 3.2 core profile and the following extensions:

    - ARB\_separate\_shader\_objects (OpenGL 4.1)
    - ARB\_vertex\_attrib\_binding (OpenGL 4.3)
    - ARB\_buffer\_storage (OpenGL 4.4)
    - ARB\_direct\_state\_access (OpenGL 4.5)

   Program introspection (currently only used for subroutines) requires:

    - ARB\_program\_interface\_query (OpenGL 4.3)

   Textures require:

    - ARB\_texture\_storage (OpenGL 4.2)
    - ARB\_texture\_storage\_multisample (OpenGL 4.3)

   Loading .gltf models requires:

    - ARB\_multi\_draw\_indirect (OpenGL 4.5)

 * An x86-64 CPU with the AVX extension

Optional dependencies:

 * [Ahven 2][url-ahven] if you want to build and run the unit tests

 * [GLFW 3][url-glfw] for the GLFW bindings

Compilation
-----------

A Makefile is provided to build the source code and examples. Use `make` to build
the source code:

    $ make

If you want to check after each call to OpenGL whether an error flag was set
and raise a corresponding exception, then use the `development` mode:

    $ MODE=development make

The default mode is `release`. Both `release` and `development` enable general
optimizations. To enable OpenGL exceptions, disable optimizations, and include
debugging symbols, use the `debug` mode.

Examples
--------

The project contains some examples that demonstrate the basic usage of
the library. Build the example programs as follows:

    $ make examples

You can execute them in the `bin` directory. Some examples load shader
files from the source directory by using relative paths, so they only work
with `bin` as the current directory.

Tests
-----

The project contains a set of unit tests. Use `make test` to build the unit tests:

    $ make test

After having build the tests, run the unit tests:

    $ make run_unit_tests

Installation
------------

After having compiled the source code, the library can be installed by executing:

    $ make PREFIX=/usr install

Change `PREFIX` to the preferred destination folder.

Using Orka in your project
--------------------------

Specify the dependency in your \*.gpr project file:

    with "orka";

If you want to use GLFW, refer to `orka-glfw` instead. The project files
`orka.gpr` and `orka-glfw.gpr` take the following scenario parameters:

 * `Windowing_System`: Sets the backend windowing system. Used for GLFW and also
                       for system-dependent parts of the API (GLX, WGL, CGL):

    - `x11`: X Windowing System (Linux, BSD, etc)
    - `windows`: Microsoft Windows
    - `quartz`: Quartz Compositor (OS X)

 * `Mode`: May take one of the following values:

    - `debug`: Compile the project with debugging symbols and OpenGL
      exceptions, and without optimizations.
    - `development`: Compile the project with optimizations, but enable
      OpenGL exceptions.
    - `release` (default): Compile the project for a release environment;
      OpenGL exceptions are disabled and optimizations are enabled.

 * `GLFW_Lib`: Linker flags for GLFW. The default is `-lglfw`.

License
-------

The OpenGL and GLFW bindings and Orka are distributed under the terms
of the [Apache License 2.0][url-apache].

  [url-openglada]: https://github.com/flyx/OpenGLAda
  [url-glfw]: http://www.glfw.org/
  [url-adacore]: http://libre.adacore.com/
  [url-fsf]: https://gcc.gnu.org/wiki/GNAT
  [url-gprbuild]: http://www.adacore.com/gnatpro/toolsuite/gprbuild/
  [url-ahven]: http://ahven.stronglytyped.org
  [url-apache]: https://opensource.org/licenses/Apache-2.0
