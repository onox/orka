[![License](https://img.shields.io/:license-Apache_License_2.0-blue.svg)](https://github.com/onox/orka/blob/master/LICENSE.md)

Orka
====

Orka is the OpenGL Rendering Kernel in Ada. It is written in Ada 2012
and provides an object-oriented API for modern OpenGL. Orka and the OpenGL
bindings require and use OpenGL 4.5's Direct State Access (DSA) extension.

Orka builds upon and provides thick bindings for OpenGL 4.5. These bindings
are based on the original [OpenGLAda][url-openglada] bindings. Bindings for
the fixed function functionality have been removed and bindings for various
extensions of OpenGL 4.x have been added.

Additionally, it provides bindings for [GLFW 3.x][url-glfw]. This is a library
for creating windows with an OpenGL context on them. It also provides
functionality for capturing user input on keyboard, mouse and joystick.
Having a window with an OpenGL context is the prerequisite for using any
OpenGL functionality.

Dependencies
------------

In order to build Orka or the OpenGL bindings, you need to have:

 * A GNAT compiler that supports Ada 2012 (a GPL'd version is available
   on [AdaCore's Libre Site][url-adacore], or you can use the
   [version provided by the FSF with GCC][url-fsf])
 * [GPRBuild][url-gprbuild] (is bundled with AdaCore's GNAT distribution).
   Minimum supported version is the one that comes with GNAT GPL 2012. Do
   not use `gnatmake` to build the project files, it won't work.
 * An OpenGL implementation that supports the core profile of OpenGL (3.2 or higher)
   and the following extensions:

    - ARB\_separate\_shader\_objects (OpenGL 4.1)
    - ARB\_vertex\_attrib\_binding (OpenGL 4.3)
    - ARB\_direct\_state\_access (OpenGL 4.5)
 * Optionally [GLFW][url-glfw]

Note: You may also be able to build the project with another Ada compiler and/or
without using the \*.gpr files. You just have to import the sources to your
compiler apart from GNAT, so if I accidentally used some GNAT-specific features
in the code, please drop me a message.

Compilation
-----------

A Makefile is provided to build the source code and tests. Use `make` to build
the source code:

    $ make

Use `make test` to build the test programs:

    $ make test

Using Orka in your project
--------------------------

Specify the dependency in your \*.gpr project file:

    with "orka";

If you want to use GLFW or just the OpenGL bindings, refer to `opengl-glfw`
or `opengl` instead.

The project files `opengl.gpr` and `opengl-glfw.gpr` take the following
scenario parameters:

 * `Windowing_System`: Sets the backend windowing system. Used for GLFW and also
                       for system-dependent parts of the API (GLX, WGL, CGL):

    - `x11`: X Windowing System (Linux, BSD, etc)
    - `windows`: Microsoft Windows
    - `quartz`: Quartz Compositor (OS X)

 * `mode`: May take one of the following values:

    - `debug`: Compile the project with debugging symbols and without
      optimization.
    - `release` (default): Compile the project for a release environment.

 * `Auto_Exceptions`: Configures exception handling:

   - `enabled` (default): After each call to OpenGL, the code checks whether
     OpenGL has set an error flag and if it had, raises the corresponding
     exception.
   - `disabled`: The user has to query the error flag on his own.

 * `GLFW_LIB`: Linker flags for GLFW. The default is `-lglfw`.

If you choose not to use GPRBuild, you need to add the sources to your
project and then use whatever build system you want.
Just make sure that you link properly against your OpenGL implementation:

 * OS X: `-framework OpenGL -framework CoreFoundation`
 * Windows: `-lOpenGL32 -lGdi32`
 * X11-based (Linux, BSD, etc): `-lGL -lX11`

If you're using GLFW, add `-lglfw3` or `-lglfw`. If you're
on Windows and link against GLFW as a dynamic library, you also need to add
`-lwinmm`.

Tests
-----

The project contains some tests. You can also see them as examples
that demonstrate the basic usage of the API. After building them as described
above, you can execute them in the `bin` directory. Some tests load shader
files from the source directory by using relative paths, so they only work with
`bin` as working directory.

License
-------

The OpenGL and GLFW bindings (`src/gl`, `src/glfw`, and `test/gl`) are
distributed under the terms of the [ISC License][url-isc].
Orka (`src/orka`) is licensed under the [Apache License 2.0][url-apache].

  [url-openglada]: https://github.com/flyx/OpenGLAda
  [url-glfw]: http://www.glfw.org/
  [url-adacore]: http://libre.adacore.com/
  [url-fsf]: https://gcc.gnu.org/wiki/GNAT
  [url-gprbuild]: http://www.adacore.com/gnatpro/toolsuite/gprbuild/
  [url-isc]: https://opensource.org/licenses/ISC
  [url-apache]: https://opensource.org/licenses/Apache-2.0
