# Installation

## Dependencies

In order to build and use Orka you need to have:

 * Ada 2012 compiler ([GNAT FSF][url-fsf] from
   your Linux distribution or [GNAT CE][url-ce])

 * [Alire][url-alire]

 * Video driver with [EGL][url-egl] 1.5 (or 1.4 + extensions below) or WGL,
   and OpenGL 4.6 core profile (or 4.0 + extensions below)

    ??? summary "Required EGL extensions"
        Following table shows a list of client extensions:

        | Client extension                      |
        |---------------------------------------|
        | EGL_KHR_debug                         |
        | EGL_KHR_client_get_all_proc_addresses |
        | EGL_EXT_client_extensions             |
        | EGL_EXT_device_enumeration            |
        | EGL_EXT_device_query                  |
        | EGL_EXT_platform_base                 |
        | EGL_EXT_platform_device               |

        Following table shows a list of platform extensions:

        | Platform extension                    |
        |---------------------------------------|
        | EGL_KHR_create_context                |
        | EGL_KHR_no_config_context             |
        | EGL_KHR_surfaceless_context           |

    ??? summary "Required OpenGL extensions"
        Following table shows a list of extensions that are part of OpenGL 4.x.
        Video drivers which do not officially support OpenGL 4.6 might
        still support a subset of the extensions listed below.

        | Extension                            | OpenGL | Required for  |
        |--------------------------------------|--------|---------------|
        | ARB\_shader\_draw\_parameters        | 4.6    | glTF          |
        | ARB\_direct\_state\_access           | 4.5    |               |
        | KHR\_context\_flush\_control         | 4.5    |               |
        | ARB\_clip\_control                   | 4.5    | Reversed Z    |
        | ARB\_get\_texture\_sub\_image        | 4.5    | KTX writer    |
        | ARB\_buffer\_storage                 | 4.4    |               |
        | ARB\_clear\_texture                  | 4.4    | Textures      |
        | KHR\_debug                           | 4.3    | Debugging     |
        | ARB\_clear\_buffer\_object           | 4.3    |               |
        | ARB\_compute\_shader                 | 4.3    |               |
        | ARB\_copy\_image                     | 4.3    | Textures      |
        | ARB\_multi\_draw\_indirect           | 4.3    | glTF          |
        | ARB\_shader\_storage\_buffer\_object | 4.3    |               |
        | ARB\_program\_interface\_query       | 4.3    |               |
        | ARB\_internalformat\_query2          | 4.3    | KTX writer    |
        | ARB\_texture\_storage\_multisample   | 4.3    | Textures      |
        | ARB\_texture\_storage                | 4.2    | Textures      |
        | ARB\_shader\_image\_load\_store      | 4.2    | Barriers      |
        | ARB\_map\_buffer\_alignment          | 4.2    |               |
        | ARB\_separate\_shader\_objects       | 4.1    |               |

 * An x86-64 CPU with the AVX and F16C extensions

Optional dependencies:

 * `lcov` to generate a coverage report for the unit tests

 * `make` (on Linux)

!!! info "SDL 2"
    Although the use of [AWT][url-awt] is recommended, alternatively
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

=== ":material-ubuntu: Ubuntu"

    ```sh
    $ apt install gnat gprbuild make
    ```

    After having installed the compiler, install the [Alire][url-alire]
    package manager.

=== ":material-arch: Arch Linux"

    ```sh
    $ pacman -S gcc-ada make
    ```

    Compile and install `gprbuild-bootstrap`, `xmlada`, `libgpr`, and then `gprbuild`
    from the AUR.
    After having installed the compiler, install the [Alire][url-alire]
    package manager.

=== ":material-microsoft-windows: Windows 10"

    Install [GNAT CE][url-ce] and [Alire][url-alire] package manager.

## Using Orka in your application

Add Orka to your application:

```sh
$ alr with orka
```

You may need to add `--use=path/to/orka/crate` to use unpublished versions.

To create an OpenGL context and window, add `awt` for AWT or `orka_plugin_sdl` for SDL.

###  Tools and examples

The project provides tools to view glTF models, KTX textures, and to
display the OpenGL version and list the available extensions:

```sh
$ make tools
```

and then go to the `orka_tools` folder and run one of the provided executables
from there. See `alr run --list` for a list.

The project contains some examples that demonstrate the basic usage of
the library. Build the example programs as follows:

```sh
$ make examples
```

and then go to the `examples` folder and run one of the provided executables
from there.

### Running tests

The project contains a set of unit tests. Use `make tests` to build and
run the unit tests. A coverage report can be generated if `lcov` is installed:

```sh
$ make clean
$ make tests
$ make coverage
$ make clean
```

## Rendering your first triangle

After having installed Orka, you might want to skip the next few chapters
and jump to [Rendering](/rendering) to get an introduction to rendering
and learn how you can render your first triangle on the screen.

  [url-alire]: https://alire.ada.dev/
  [url-awt]: https://github.com/onox/orka/tree/master/awt
  [url-ce]: http://libre.adacore.com/
  [url-fsf]: https://gcc.gnu.org/wiki/GNAT
  [url-sdlada]: https://github.com/Lucretia/sdlada
  [url-egl]: https://www.khronos.org/egl
  [url-gcn]: https://en.wikipedia.org/wiki/Radeon#Feature_overview
  [url-hd]: https://en.wikipedia.org/wiki/Intel_Graphics_Technology#Capabilities
