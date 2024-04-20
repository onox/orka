# Shaders

Shaders are programs running a single stage of the graphics or compute
pipeline on the GPU. They operate on a vertex, some geometry, or a
fragment (a pixel on the screen).
Compute shaders, which form the the one and only stage of a compute pipeline,
can read or write to buffers and textures arbitrarily.

Pipelines containing vertex and fragment shaders are launched by draw commands,
while pipelines containing compute shaders are dispatched.

!!! info
    The various objects described on this page are declared in
    the package `Orka.Rendering.Shaders` and its child packages.

## Creating a shader

To create a shader, call the function `Create_Shader` in the child package `:::ada Objects`,
giving it the desired shader kind, and the path to a file in the given location:

```ada
Pipeline_1 : Shader_Objects :=
  (Vertex_Shader   => Create_Shader (Location_Shaders, Vertex_Shader, "example.vert"),
   Fragment_Shader => Create_Shader (Location_Shaders, Fragment_Shader, "example.frag"),
   others          => Empty);
```

In this case `Pipeline_1` contains a vertex and a fragment shader.
Leave unused stages of the pipeline empty by using the function `Empty`
in the child package `:::Objects`.

There are 4 ways to create a shader:

1. From a file by using the function `Create_Shader`.

2. From multiple files by using the function `Create_Shader_From_Files`. In this case
   the parameter `Paths` must be given an array of pointers to `aliased constant String`,
   each containing the path to a file.

3. From source text by using the function `Create_Shader_From_Source`. This function is useful
   if you need to programmatically construct the text of a shader.

4. From one or more modules by using the function `Create_Shader`, giving it a value
   of the type `Shader_Module_Array` (defined in the child package `:::ada Modules`),
   which is an array of `Shader_Module`. Each module contains a part of the shader
   and can be created from a file or from text.

## Modules

A module can be created from one or more files or from text. This allows you to create a shader
whose code comes partially from files and partially from text.
This can be useful if you need to programmatically construct a shader.

A `Shader_Module` can be created by calling one of the appropriate functions
from the child package `:::ada Modules`. For example:

```ada
Module_1 : Shader_Module_Array := Create_Modules
  (Shader_Location, Fragment_Shader, [File_1'Access, File_2'Access]);
```

where `File_1` and `File_2` are each an `aliased constant String` containing the path to a file.

In this example `Shader_Location` is a pointer to an object implementing
the interface `Location`. The location object is responsible for retrieving
the files given in the array.
Implementations exist which can read files from a directory or from an archive file.
See [Locations](/resources/locations/) for more information.

There are 3 ways to create module:

1. From a file by using the function `Create_Module`.
   This function returns a `Shader_Module`.

2. From multiple files by using the function `Create_Modules`. In this case
   the parameter `Paths` must be given an array of pointers to `aliased constant String`,
   each containing the path to a file.
   This function returns a `Shader_Module_Array`, not a single `Shader_Module`.

3. From source text by using the function `Create_Module_From_Source`. This function is useful
   if you need to programmatically construct the text of a shader.
   This function returns a `Shader_Module`.

After one or more modules have been created, use the function `Create_Shader` in the child
package `:::ada Objects` to create the actual shader.

!!! note "Use modules only if the code comes from files *and* text"
    If a shader's code comes from only one or more files, or only from
    source text, then just use the functions in the child package `:::ada Objects`
    to create a shader. In that case there is no need to create modules first.

## Uniforms

Shaders may contain uniforms. A uniform is a variable that can be read by a shader
and written to from Ada code prior to running the shaders of a pipeline.
Uniforms should contain a small amount of data.
For a larger amount of data, like a few matrices for cameras, a buffer binded
as a [UBO](/rendering/buffers/#ubo) can be used.
Very large buffers must be binded as an [SSBO](/rendering/buffers/#ssbo).

A uniform can be retrieved using the function `Uniform`.
It return a type `Uniform` from the child package `:::ada Uniforms`:

```ada
Uniform_View : Uniform := Pipeline_1 (Vertex_Shader).Value.Uniform ("view");
Uniform_Proj : Uniform := Pipeline_1 (Vertex_Shader).Value.Uniform ("proj");
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

!!! tip "Use a UBO for multi-stage uniforms"
    If a uniform is used in multiple stages, then multiple `Uniform` objects
    should be created and given a value.
    In this case it can be easier to create a buffer and bind it as a [UBO](/rendering/buffers/#ubo).

### Textures and images

For textures and images it is sufficient to bind them by calling
`:::ada Orka.Rendering.Textures.Bind`.

If it is needed to get the uniform of a texture or image so that
it can be verified that the kind and format of the sampler and texture
are compatible, call the functions `Uniform_Sampler` or `Uniform_Image`:

```ada
Uniform_Texture : Uniform_Sampler :=
  Pipeline_1 (Fragment_Shader).Value.Uniform_Sampler ("diffuseTexture");
```

Type `Uniform_Sampler` provides the procedure `Verify_Compatibility` to
verify the kind and format:

```ada
Uniform_Texture.Verify_Compatibility (Texture_1);
```

## Using a set of shaders

To use a set of shaders stored in a `Shader_Objects` array before executing
a rendering command, call the procedure `Bind_Shaders` of the current `Context`:

```ada
Context.Bind_Shaders (Pipeline_1);
```

After a set of shaders have been made active, rendering commands can be issued
to render to a framebuffer.
