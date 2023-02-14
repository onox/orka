# Textures

Textures are objects which present images with one, two, or three
dimensions, and one or multiple layers. Additionally, they can have
multiple mipmap levels, which contain lower resolution variants of
the image.

!!! info
    The various objects described on this page are declared in
    the packages `:::ada Orka.Rendering.Textures`, `:::ada GL.Objects.Textures`
    and `:::ada GL.Objects.Samplers`.

## Creating a texture

A texture can be created by declaring it:

```ada
Texture_1 : Texture (GL.Low_Level.Enum.Texture_2D);
```

After the object has been created, its storage must be allocated:

```ada
Texture_1.Allocate_Storage (1, 0, GL.Pixels.RGBA8, Width, Height, 1);
```

The first parameter specifies the `Levels` and the second the number of `Samples`.
After storage has been allocated, the texture can be attached to a framebuffer.
See [Attaching textures](/rendering/framebuffers/#attaching-textures)
for more information on how to attach a texture.

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
Sampler_1 : Sampler;
```

And then modify its state as needed:

```ada
Sampler_1.Set_X_Wrapping (Clamp_To_Edge);
Sampler_1.Set_Y_Wrapping (Clamp_To_Edge);

Sampler_1.Set_Minifying_Filter (Linear_Mipmap_Linear);
Sampler_1.Set_Magnifying_Filter (Linear);
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
Uniform_1 : Uniform_Sampler := Program_1.Uniform_Sampler ("matrixBuffer");

Uniform_1.Verify_Compatibility (Buffer_Texture_1);
```
