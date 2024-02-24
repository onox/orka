# Textures

Textures are objects which present images with one, two, or three
dimensions, and one or multiple layers. Additionally, they can have
multiple mipmap levels, which contain lower resolution variants of
the image.

!!! info
    The various objects described on this page are declared in
    the packages `:::ada Orka.Rendering.Textures` and `:::ada Orka.Rendering.Samplers`.

## Creating a texture

A texture can be created by calling function `Create_Texture`:

```ada
Texture_1 : Texture (Textures.LE.Texture_2D) := Create_Texture
  ((Kind     => Textures.LE.Texture_2D_Multisample,
    Format   => GL.Pixels.R11F_G11F_B10F,
    Size     => (1280, 720, 1),
    Levels   => 1,
    Samplers => 2));
```

After the texture has been created, it can be attached to a framebuffer.
See [Attaching textures](/rendering/framebuffers/#attaching-textures)
for more information on how to attach a texture.

The depth (third parameter of `Size`) must be a multiple of 6 if the
texture is a cube map array.

### Creating a view of a layered texture

Some textures are considered layered, meaning they have an additional
dimension representing the layers of the texture.
For example, a `Texture_2D_Array` is a layered texture containing an array
of 2D textures.

The function `Create_View` can be used to create a texture whose data
points to a single layer of another texture:

```ada
Texture_2 : Texture := Texture_Cube.Create_View (Layer => 5);
```

In this example `Texture_2` is a 2D texture representing the face *-Z* of
the cube map texture `Texture_Cube`.

Textures with the following kinds are layered textures and can be used by the
function `Create_View`:

* `Texture_1D_Array`
* `Texture_2D_Array`
* `Texture_2D_Multisample_Array`
* `Texture_3D`
* `Texture_Cube_Map`
* `Texture_Cube_Map_Array`

## Getting the description

The description used to create a texture can be retrieved by calling the
function `Description`.

## Downloading data

!!! note "TODO"

## Uploading data

!!! note "TODO"

## Clearing pixels

!!! note "TODO"

## Copying pixels to another texture

!!! note "TODO"

## Binding textures

A texture can be binded to a binding point so that it can be read and/or
written in a shader.

### As a readonly texture

A texture can be binded as a readonly texture by calling the procedure `Bind`:

```ada
Texture_2.Bind (0);
```

It can then be read in a shader by declaring a uniform sampler using the same
binding point for the layout parameter `binding`:

```glsl
layout(binding = 0) uniform sampler2D inputTexture;
```

and then by reading the texture with the function `:::glsl texture()`:

```glsl
const vec4 pixel = texture(inputTexture, textureCoordinate);
```

where `textureCoordinate` is a normalized coordinate unless the texture is a `sampler2DRect`.
If it is then it must contain the pixel column and row instead.

### As a writable image

A texture can be binded as a writable image by calling the procedure `Bind_As_Image`:

```ada
Texture_3.Bind_As_Image (1);
```

In a shader it must then be declared as a uniform image. For example, an image that is
only written by a shader can be declared as follows:

```glsl
layout(binding = 1) volatile restrict writeonly uniform image2D outputImage;
```

If a texture is binded as an image, the functions `:::glsl imageLoad()` and
`:::glsl imageStore()` must be used instead.
For example, a color can be written to a pixel as follows:

```glsl
imageStore(outputImage, ivec2(column, row), color);
```

where the variable `color` has the type `vec4`.

## Saving to a KTX file

To save a `Texture` object to a file, the procedure `Write_Texture`
in the package `:::ada Orka.Resources.Textures.KTX` can be used to write
the texture to a .ktx file in a writable location:

```ada
Orka.Resources.Textures.KTX.Write_Texture
  (Texture_1, Location_Screenshots, "screenshot.ktx");
```

The location given to the procedure must implement the interface `Writable_Location`.
See [Directories](/resources/locations/#directories) on how to create such
an object.

!!! tip "View the saved .ktx file with the orka\_ktx tool"
    The executable orka\_ktx of the Alire crate orka\_tools can display
    textures in .ktx files.

## Samplers

A sampler specifies how a texture should be sampled.
Create a sampler by declaring it:

```ada
use all type Orka.Rendering.Samplers.Minifying_Function;
use all type Orka.Rendering.Samplers.Magnifying_Function;
use all type Orka.Rendering.Samplers.Wrapping_Mode;

Sampler_1 : Sampler := Create_Sampler
  ((Wrapping          => (others => Clamp_To_Edge),
    Minifying_Filter  => Linear_Mipmap_Linear,
    Magnifying_Filter => Linear,
    others            => <>));
```

### Using a sampler in a shader

To use a sampler in a shader, bind it to the index of a binding point:

```ada
Sampler_1.Bind (0);
```

The binding point must be specified in the shader as well:

```glsl
layout(binding = 0) uniform sampler2D diffuseTexture;
```

### Verify compatibility

To verify that the kind and format of the sampler and texture are
compatible, call procedure `Verify_Compatibility`:

```ada
declare
   Uniform_1 : Uniform_Sampler := Program_1.Uniform_Sampler ("matrixBuffer");
begin
   Uniform_1.Verify_Compatibility (Buffer_Texture_1);
end;
```

Since `Verify_Compatibility` is a null procedure, make sure to compile
with assertions enabled by building with `ADAFLAGS="-gnata".
