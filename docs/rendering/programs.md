# Programs

Programs are objects which run one or more shaders on the GPU.
A shader is a stage of the program that operates on a vertex, some
geometry, or a fragment (a pixel on the screen).
Programs that wish to read or write to buffers and textures arbitrarily
use compute shaders.

Progams containing vertex and fragment shaders are launched by draw commands,
while programs containing compute shaders are dispatched.

!!! info
    The various objects described on this page are declared in
    the package `Orka.Rendering.Programs` and its child packages.

## Creating a program

To create a program, call function `Create_Program`, giving it a value of
the type `Module` (defined in the child package `:::ada Modules`):

```ada
Program_1 : Program := Create_Program (Module_1);
```

In this case `Module_1` should contain either a vertex shader and a fragment shader,
or just a compute shader.

Another way is to create a program from multiple modules by giving `Create_Program`
a value of the type `Module_Array` (also defined in the child package `:::ada Modules`):

```ada
Program_2 : Program := Create_Program (Modules.Module_Array'
  (Module_VS, Module_FS));
```

## Modules

Modules contain one or more shaders, which are all retrieved from files
or from inline text.
A module is created by the function `Create_Module` from the package
`:::ada Orka.Rendering.Programs.Modules`:

```ada
Module_1 : Modules.Module := Modules.Create_Module
  (Shader_Location,
   VS => "tools/gltf.vert",
   FS => "tools/gltf.frag")
```

In this example `Shader_Location` is a pointer to an object implementing
the interface `Location`. The location object is responsible for retrieving
the files given by the parameters `VS` and `FS`
(for a vertex and fragment shader).
Implementations exist which can read files from a directory or from an archive file.
See [Locations](/resources/locations/) for more information.

For the example which creates `Program_2`, the modules are created separately using calls to
the function `Create_Module`:

```ada
Module_VS : Modules.Module :=
  Modules.Create_Module (Shader_Location, VS => "oversized-triangle.vert");
```

To create a module from text instead of a file, use `Create_Module_From_Sources`:

```ada
Module_FS : Modules.Module :=
  Modules.Create_Module_From_Sources (FS => Text_Of_Fragment_Shader);
```

This can be useful if you need to programmatically construct a shader.

## Uniforms

Programs may contain uniforms. A uniform is a variable that can be read by a shader
and written to from Ada code prior to running the program.
Uniforms should contain a small amount of data.
For a larger amount of data, like a few matrices for cameras, a buffer binded as a UBO can be used.
Very large buffers must be binded as an SSBO.

A uniform can be retrieved using the function `Uniform`.
It return a type `Uniform` from the child package `:::ada Uniforms`:

```ada
Uniform_View : Uniforms.Uniform := Program_1.Uniform ("view");
Uniform_Proj : Uniforms.Uniform := Program_1.Uniform ("proj");
```

In the shader, the uniforms are declared as following:

```glsl
uniform mat4 view;
uniform mat4 proj;
```

and then used in the `main()` function of the shader:

```glsl
gl_Position = proj * view * some_vertex;
```

!!! warning "Retrieving an unused uniform may raise an exception"
    If a uniform is defined by a shader, but is unused, retrieving it
    may raise the exception `:::ada Uniforms.Uniform_Inactive_Error`.

### Textures and images

For textures and images it is sufficient to bind them by calling
`:::ada Orka.Rendering.Textures.Bind`.

If it is needed to get the uniform of a texture or image so that
it can be verified that the kind and format of the sampler and texture
are compatible, call the functions `Uniform_Sampler` or `Uniform_Image`:

```ada
Uniform_Texture : Uniforms.Uniform_Sampler :=
  Program_1.Uniform_Sampler ("diffuseTexture");
```

Type `Uniform_Sampler` provides the procedure `Verify_Compatibility` to
verify the kind and format:

```ada
Uniform_Texture.Verify_Compatibility (Texture_1);
```

## Using a program

To use a program before executing a rendering command, call the
procedure `Use_Program`:

```ada
Program_1.Use_Program;
```

After a program has been made active, rendering commands can be issued
to render to a framebuffer.
