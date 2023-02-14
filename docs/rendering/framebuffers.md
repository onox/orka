# Framebuffers

A framebuffer is an object that holds the output of a program that ran
on the GPU. Usually it consists of a color buffer and a depth buffer.
An application always has at least one framebuffer: the default framebuffer,
which is the window of the application. For off-screen rendering, needed
for post-processing, additional framebuffers can be used.

A framebuffer has one or more color buffers, and optionally a depth and/or
a stencil buffer.

!!! info
    The various objects described on this page are declared in
    the package `:::ada Orka.Rendering.Framebuffers`.

## Creating a framebuffer

A framebuffer object can be created by calling the function `Create_Framebuffer`:


```ada
Framebuffer_1 : Framebuffer := Create_Framebuffer (Width, Height, Samples);
```

The given parameters are stored so that when textures are attached (explained below),
it can be verified whether they are compatible.

The default framebuffer can be created with the function `Create_Default_Framebuffer`:

```ada
Framebuffer_Default : Framebuffer :=
  Create_Default_Framebuffer (Window.Width, Window.Height);
```

A framebuffer has one discriminant named `Default`, which specifies whether it is
the default framebuffer or a regular framebuffer object. Thus a framebuffer object
can be stored in a record as following:

```ada
type Record_1 is record
   Framebuffer_1 : Orka.Rendering.Framebuffers.Framebuffer (Default => False); 
end record;
```

The width, height, and samples of a framebuffer can be queried with the functions
`Width`, `Height`, and `Samples`.

## Attaching textures

To give the framebuffer a color, depth, or stencil buffer, a texture must
be attached to the framebuffer. At most one depth or stencil buffer can be
attached.
A framebuffer supports up to 8 color buffers, but usually a framebuffer has
just one.

To add a texture to the first color buffer, to the depth buffer, or to the
stencil buffer, call the procedure `Attach`:

```ada
Framebuffer_1.Attach (Texture_Color_1);
Framebuffer_1.Attach (Texture_Depth);
```

The texture is attached to the attachment point based on the internal format
of the texture or or to the given attachment point if the texture is color renderable.
The parameter `Attachment` can be used to specify the attachment point and it
has the default value `Color_Attachment_0`.
The attachment point is ignored for textures with a depth or stencil format,
since a framebuffer supports at most one buffer for these kinds.

If the texture is layered and you want to attach a specific layer,
then the procedure `Attach_Layer` should be called instead:

```ada
Framebuffer_1.Attach_Layer (Texture_Cube_Map_1, Color_Attachment_0, Layer => 5);
```

The function `Has_Attachment` can be used to query whether an attachment point
has a texture attached to it.

!!! note
    If one of the attached textures is layered (3D, 1D/2D array, cube
    map [array], or 2D multisampled array), then all attachments must
    have the same kind.

!!! note
    All attachments of the framebuffer must have the same amount of
    samples and they must all have fixed sample locations, or none of
    them must have them.

### Detaching a texture

To detach a texture from an attachment point, call the procedure `Detach`:

```ada
Framebuffer_1.Detach (Color_Attachment_7);
```

## Clearing the attached buffers

Every frame the buffers should be cleared. To clear the buffers, call the
procedure `Clear`. Before the buffers can be cleared, the default values which
must be used when clearing must be set first by calling the procedure
`Set_Default_Values`:

```ada
Framebuffer_1.Set_Default_Values ((Color => (1.0, 0.0, 0.0, 1.0), others => <>));
```

In this case the color of the color buffer after clearing will be red.
Procedure `Set_Default_Values` needs to be called only once. It is not
needed to call it every frame.
A good place to call it is right after creating the framebuffer.

The current default values can be queried with the function `Default_Values`.

After the default values have been set, the buffers can and should be cleared
before rendering geometry by calling the procedure `Clear`.
For example, the following statement will clear the color and depth buffer:

```ada
Framebuffer_1.Clear ((Color | Depth => True, others => False));
```

The pixels in the buffers will be reset to the values previously set by `Set_Default_Values`.

!!! note
    The default value of `Depth` is `0.0` because Orka uses a reversed depth
    buffer (reversed Z) for greater precision of geometry at great distances
    from the camera.

## Using a framebuffer

To use a framebuffer so that rendering geometry will be drawn on the attached
color textures, and the depth of each fragment (pixel) is stored in the attached
depth texture, call the procedure `Use_Framebuffer`:

```ada
Framebuffer_1.Use_Framebuffer;
```

If an application uses only the default framebuffer, then this procedure does not
need to be called. If an application uses the default framebuffer and one or more
regular framebuffers, then, each frame, `Use_Framebuffer` must be called before
using each framebuffer.

## Scaling and resolving multiple samples

If a texture must be scaled down to a lower resolution, or if it has multiple samples
which must be resolved, then the way to do this is to attach the source and destination
textures to two different framebuffers and then use the procedure `Resolve_To` to copy
the content of the color, depth, or stencil buffers to the second framebuffer.
This process is called *blitting*.

If a buffer is specified in the mask, then the buffer should exist
in both framebuffers, otherwise the buffer is not copied. Call
`Set_Read_Buffer` and `Set_Draw_Buffers` to control which buffer is read
from and which buffers are written to.
If the framebuffers have just one color buffer then these procedures do not
need to be called.

Format of the color buffers may differ and will be converted (if supported).
However, formats of depth and stencil buffers must match.

For example, to blit the color buffer from `Framebuffer_1` to `Framebuffer_2`,
call `Resolve_To` as following:

```ada
Framebuffer_1.Resolve_To (Framebuffer_2);
```

!!! warning
    Simultaneously resolving multiple samples and scaling
    of color buffers requires `EXT_framebuffer_multisample_blit_scaled`.
    If this extension is not present, then two separate calls to `Resolve_To` are needed.
