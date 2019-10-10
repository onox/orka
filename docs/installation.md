# Installation

## Dependencies

In order to build and use Orka you need to have:

 * Ada 2012 compiler (Either [GNAT FSF][url-fsf] from
   your Linux distribution or [GNAT CE][url-ce])

 * GPRBuild and `make`

 * [json-ada 3.0.2][url-json-ada] (for glTF)

 * [dcf-ada 2.0.0][url-dcf-ada] (for loading resources in Zip archives)

 * Video driver with [EGL 1.5][url-egl] or WGL, and OpenGL 4.6 core profile
   (or 4.0 + extensions below)

    ??? summary "Required extensions in OpenGL 4.x"
        Following table shows a list of extensions that are part of OpenGL 4.x.
        Video drivers which do not officially support OpenGL 4.6 might
        still support a subset of the extensions listed below.

        | Extension                            | OpenGL | Required for  |
        |--------------------------------------|--------|---------------|
        | ARB\_shader\_draw\_parameters        | 4.6    | glTF          |
        | ARB\_direct\_state\_access           | 4.5    |               |
        | ARB\_clip\_control                   | 4.5    | Reversed Z    |
        | ARB\_get\_texture\_sub\_image        | 4.5    | KTX writer    |
        | ARB\_buffer\_storage                 | 4.4    |               |
        | ARB\_clear\_texture                  | 4.4    | Textures      |
        | KHR\_debug                           | 4.3    | Debugging     |
        | ARB\_clear\_buffer\_object           | 4.3    |               |
        | ARB\_compute\_shader                 | 4.3    | Culling       |
        | ARB\_copy\_image                     | 4.3    | Textures      |
        | ARB\_multi\_draw\_indirect           | 4.3    | glTF          |
        | ARB\_shader\_storage\_buffer\_object | 4.3    | glTF, culling |
        | ARB\_program\_interface\_query       | 4.3    | Subroutines   |
        | ARB\_internalformat\_query2          | 4.3    | KTX writer    |
        | ARB\_vertex\_attrib\_binding         | 4.3    |               |
        | ARB\_texture\_storage\_multisample   | 4.3    | Textures      |
        | ARB\_texture\_storage                | 4.2    | Textures      |
        | ARB\_shader\_image\_load\_store      | 4.2    | Culling       |
        | ARB\_map\_buffer\_alignment          | 4.2    |               |
        | ARB\_separate\_shader\_objects       | 4.1    |               |

 * An x86-64 CPU with the AVX and F16C extensions

Recommended dependencies:

 * [GLFW 3][url-glfw] for the GLFW bindings, tools, and examples

Optional dependencies:

 * [Ahven 2][url-ahven] if you want to build and run the unit tests

 * `lcov` to generate a coverage report for the unit tests

!!! info "SDL 2"
    Although the use of GLFW is recommended, alternatively
    [SDLAda][url-sdlada] can be used to manage windows and input via SDL 2.

!!! warning "CPU and OpenGL requirements"
    For the SIMD intrinsics, the x86 extensions AVX and F16C are needed.
    The following CPUs should have support for these extensions:

    - Intel Ivy Bridge Core i3 or newer (2012)
    - AMD Jaguar-based (2013) or Bulldozer-based (2011) or newer
    - AMD Zen-based (2017) or newer

    On Linux, the following GPUs should support all the required OpenGL extensions:

    - [Intel HD Graphics Gen 7][url-hd] (Ivy Bridge, 2012) or newer
    - [AMD GCN 1st gen][url-gcn] (HD 7000 series, 2012) or newer
    - Nvidia Kepler (GeForce 600 series, 2012) or newer

### Installing dependencies

#### Ubuntu 18.04 LTS

Install the dependencies using apt:

```sh
$ sudo apt install gnat-7 gprbuild make libahven6-dev lcov
$ sudo apt install libglfw3 libglfw3-dev libegl1-mesa-dev
```

Compile and install [json-ada][url-json-ada] and [dcf-ada][url-dcf-ada].

!!! tip "Tip: Install GLFW 3.3"
    It is recommended to compile and install GLFW 3.3 with Wayland support
    instead of installing the version provided in the Ubuntu repositories.

#### Arch Linux

Install the dependencies using pacman and makepkg:

```sh
$ sudo pacman -S gcc-ada glfw-wayland
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

Compile and install [json-ada][url-json-ada] and [dcf-ada][url-dcf-ada].

#### Windows 10

See #10 for instructions on how to install Orka on Windows 10.

## Installing from source

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

###  Tools and examples

The project provides tools to view glTF models, KTX textures, and to
display the OpenGL version and list the available extensions:

```sh
$ make tools
```

The project contains some examples that demonstrate the basic usage of
the library. Build the example programs as follows:

```sh
$ make examples
```

You can execute them in the `bin` directory. Some tools and examples load shader
files from the source directory using relative paths, so they only work
with `bin` as the current directory.

### Running tests

The project contains a set of unit tests. Use `make test` to build and
run the unit tests. A coverage report can be generated with `make coverage`:

```sh
$ make test
$ make coverage
```

  [url-ce]: http://libre.adacore.com/
  [url-fsf]: https://gcc.gnu.org/wiki/GNAT
  [url-ahven]: http://ahven.stronglytyped.org
  [url-json-ada]: https://github.com/onox/json-ada
  [url-dcf-ada]: https://github.com/onox/dcf-ada
  [url-glfw]: http://www.glfw.org/
  [url-sdl]: http://www.libsdl.org/
  [url-sdlada]: https://github.com/Lucretia/sdlada
  [url-egl]: https://www.khronos.org/egl
  [url-gcn]: https://en.wikipedia.org/wiki/Radeon#Feature_overview
  [url-hd]: https://en.wikipedia.org/wiki/Intel_Graphics_Technology#Capabilities
