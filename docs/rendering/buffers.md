# Buffers

Buffers are objects that contain data that can be read or written by
shaders on the GPU. Buffers must contain homogeneous data (all elements
in the buffer have the same type) and can be numeric or composite. Composite
data can be vectors, matrices, or draw commands for indirect drawing.

!!! info
    The various objects described on this page are declared in
    the package `:::ada Orka.Rendering.Buffers` and its child packages.

## Creating a buffer

To create a buffer, call `Create_Buffer`:

```ada
Buffer_1 : Buffer := Create_Buffer
  (Flags  => (Dynamic_Storage => True, others => False),
   Kind   => Orka.Types.UInt_Type,
   Length => 64);
```

`Length` specifies the number of elements in the buffer, not the number
of bytes. The size of a buffer can be queried with the function `Length`.

Alternatively, `Create_Buffer` can be called with the parameters `Flags`
and `Data` to initialize the buffer with the given data:

```ada
Indices  : Unsigned_32_Array := (1, 2, 0, 0, 2, 3);

Buffer_2 : Buffer := Create_Buffer ((others => False), Indices);
```

## Types

Buffers can contain data of one of the following types from package `:::ada Orka`:

* `Unsigned_8_Array`
* `Unsigned_16_Array`
* `Unsigned_32_Array`
* `Integer_8_Array`
* `Integer_16_Array`
* `Integer_32_Array`
* `Float_16_Array`
* `Float_32_Array`
* `Float_64_Array`

and the following types from package `:::ada GL.Types.Indirect`:

* `Arrays_Indirect_Command_Array`
* `Elements_Indirect_Command_Array`
* `Dispatch_Indirect_Command_Array`

and of the following types from `:::ada Orka.Types.Singles` and `:::ada Orka.Types.Doubles`:

* `Vector4_Array`
* `Matrix4_Array`

Additionally, for [mapped buffers](#mapped-buffers),
the non-array versions of the last five types can also be written.

## Uploading data

If data needs to be uploaded to the buffer from the CPU *after*
the buffer has been created, then `Dynamic_Storage` must be true and
`Set_Data` with the parameter `Data` can be called to upload the data:

```ada
Buffer_1.Set_Data (Indices, Offset => 42);
```

Parameter `Offset` is optional (default value is 0) and specifies the
position in the buffer of the first element of the given data.

## Downloading data

To synchronously download data, first set a barrier:

```ada
GL.Barriers.Memory_Barrier ((Buffer_Update => True, others => False));
```

and then call procedure `Get_Data`:

```ada
declare
   Data : Float_32_Array (1 .. 16) := (others => 0.0);
begin
   Buffer_0.Get_Data (Data);
end;
```

This procedure may stall the CPU.

!!! bug "Asynchronously downloading data is not yet supported"
    See issue [#32][url-github-pbo] on GitHub.

## Clearing data

Regardless of the value of `Dynamic_Storage`, data from a buffer can be
cleared with the procedure `Clear_Data`:

```ada
declare
   Data : Unsigned_32_Array := (1, 2, 0);
begin
   Buffer_2.Clear_Data (Data);
end;
```

This will write (repeatedly) 1, 2, and 0 to the buffer. To efficiently
clear the buffer with zeros, use an array with one zero:

```ada
Buffer_2.Clear_Data (Single_Array'(1 => 0.0));
```

## Copying data to another buffer

Regardless of the value of `Dynamic_Storage`, data from a buffer can be
copied to another buffer by calling the procedure `Copy_Data` on the
source buffer.

!!! tip
    Disable `Dynamic_Storage` if the buffer is only written by shaders
    on the GPU. This makes the buffer immutable and may give the video
    driver the freedom to allocate it in faster memory and/or perform
    faster validation.

    To upload data to an immutable buffer, an extra buffer with `Dynamic_Storage`
    can be created as a staging buffer. After having called `Set_Data` on
    this staging buffer, the data can be copied to the immutable buffer by
    calling `Copy_Data`.

## Binding buffers

`Buffer` objects implement the interface `Bindable_Buffer`, which provides
the procedure `Bind`. This procedure can be used to bind the buffer to a
target so that it can be used by certain operations like indirect drawing.
Valid targets are:

* `Index`
* `Dispatch_Indirect`
* `Draw_Indirect`
* `Parameter`
* `Pixel_Pack`
* `Pixel_Unpack`
* `Query`

!!! tip
    The targets can be made directly visible with
    `:::ada use all type Orka.Rendering.Buffers.Buffer_Target`.

## Accessing buffers in shaders

A second procedure `Bind` exists to bind the buffer object to the index
of a target so that the buffer can be accessed in a shader. Valid targets
are:

* `Shader_Storage` (SSBO)
* `Uniform` (UBO)

A UBO should only be used for small amount of data (no more than 64 KiB)
that is accessed uniformly by all threads of a shader. Otherwise it is
recommended to use an SSBO, which does not have these limitations.

!!! tip
    The targets can be made directly visible with
    `:::ada use all type Orka.Rendering.Buffers.Indexed_Buffer_Target`.

### SSBO

SSBOs are large writable buffers:

- Guaranteed to be at least 128 MiB. The storage size can be variable.

- Can be read and written. Writes can be atomic via
  [special functions][url-ssbo-atomics].
  A barrier is required after a shader has written to the buffer.

To use a buffer as an SSBO, create a `buffer` with a binding index in a
shader:

```glsl
layout(std430, binding = 0) buffer matrixBuffer {
    mat4 matrices[];
};
```

and then bind the buffer to the used index:

```ada
Buffer_3.Bind (Shader_Storage, 0);
```

If data has been uploaded to the buffer or data was written to it by
a shader, a memory barrier must be inserted before the buffer can be read
by another shader:

```ada
GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));
```

The buffer can then be accessed in the shader via the variable `matrices`.

!!! warning "Barriers"
    If a shader progam has written data to the buffer, you must add a
    barrier before another program or OpenGL command reads from that buffer
    again. The kind of barrier that is needed depends on how the buffer is
    subsequently *read*.

!!! info "Memory qualifiers"
    See [Memory qualifiers][url-memory-qlf] on the OpenGL Wiki for a list
    of memory qualifiers that can be added to the buffer variable.

### UBO

A uniform buffer is a buffer that provides uniform data and can be used
as an alternative to a set of separate uniforms. If several different
shader programs require the same set of uniforms, a UBO is a good fit
because it avoids having to set the same uniforms for different programs;
the buffer needs to be binded only once.

Compared to SSBOs, UBOs are severely restricted:

- UBOs are at least 16 KiB, but often 64 KiB or even 2 GiB for some vendors.
  Storage size is fixed.

- Can only be read, not written.

- Should be access uniformly by the shader invocations. May be faster
  than SSBOs.

To use a buffer as an UBO, create a `buffer` with a binding index in a shader:

```glsl
layout(std140, binding = 0) uniform cameraBuffer {
    mat4 viewTM;
    mat4 projTM;
};
```

and then bind the buffer to the used index:

```ada
Buffer_3.Bind (Uniform, 0);
```

!!! warning "Padding"
    Note that the `std140` layout pads vectors to 16 bytes (vec4). You should
    avoid using vec3. See [Memory layout][url-memory-layout] on the OpenGL Wiki.

### TBO

A third way to access a buffer in a shader is as a TBO. Data is fetched
in the shader via a texture unit, which can do format conversion in hardware.
In order to use a buffer as a TBO, the buffer must be attached to a
`Buffer_Texture` object (a special kind of texture):

```ada
Buffer_Texture_1.Attach_Buffer (GL.Pixels.RGBA32F, Buffer_3.GL_Buffer);
```

Furthermore, a uniform with an explicit binding index must be declared
in the shader:

```glsl
layout(binding = 0) uniform samplerBuffer matrixBuffer;
```

and the buffer texture must be binded to this binding point:

```ada
declare
   use all type Orka.Rendering.Textures.Indexed_Texture_Target;
begin
   Orka.Rendering.Textures.Bind (Buffer_Texture_1, Texture, 0);
end;
```

Data can then be fetched via the `texelFetch` function in the shader.

## Mapped buffers

Mapped buffers are buffers that can be read from or written to from any
task, not just the task for which the OpenGL context is current, which is
what is normally required for any OpenGL subprogram. Creating and deleting
these buffers must still happen in the rendering task, just like any other
OpenGL object.

The package `:::ada Orka.Rendering.Buffers.Mapped` contains the type `Mapped_Buffer`,
which is used and extended by all implementations of mapped buffers.
The type `Mapped_Buffer` has two discriminants: `Kind` and `Mode`.
If a record type containing a mapped buffer is needed,
the type and the component containing the mapped buffer can be declared as follows:

```ada
type Record_1 is record
   Component_1 : Some_Mapped_Buffer
     (Kind => Orka.Types.Single_Matrix_Type,
      Mode => Orka.Rendering.Buffers.Mapped.Write);
end record;
```

Just like a regular `Buffer`, a `Mapped_Buffer` can be binded to a target or
to a binding point with the procedure `Bind`.

### Writing and reading data

Data can be written to the buffer with procedure `Write_Data` if discriminant
`Mode` has the value `Write` and read with procedure `Read_Data` if the
discriminant equals `Read`.

To write data, call `Write_Data`:

```ada
Buffer_3.Write_Data (Matrix, Offset => Instance_Index);
```

To read elements from a mapped buffer, create an array on the stack
and then call `Read_Data`:

```ada
declare
   Data : Integer_32_Array (1 .. 16) := (others => 0);
begin
   Buffer_4.Read_Data (Data);
end;
```

### Persistent mapped buffers

Persistent mapped buffers are buffers that are and remain mapped
indefinitely (until the buffer is deleted). This
kind of mapped buffer is useful for data that is updated every frame.

Because the mapping is persistent, the GPU may read from or write to
the buffer while it is mapped. To guarantee mutually exclusive access
between the GPU and the CPU, the buffer must be split into multiple
regions and fences must be used to make sure the GPU and CPU never
operate on the same region.

To create a persistent mapped buffer, use the function `Create_Buffer`
in package `:::ada Orka.Rendering.Buffers.Mapped.Persistent`:

```ada
Buffer_4 : Persistent_Mapped_Buffer := Create_Buffer
  (Kind    => Orka.Types.Single_Matrix_Type,
   Length  => 1024,
   Mode    => Orka.Rendering.Buffers.Mapped.Write,
   Regions => 3);
```

At the end of a frame, after reading or writing to the buffer,
the buffer can be set to the next region:

```ada
Buffer_4.Advance_Index;
```

Note that this procedure does not set or wait for any fence.
See [Fences](#fences) on how to set and wait for a fence.

#### Writing data

If it is intended to write to the buffer, as indicated by setting `Mode`
to `Write`, then you must wait for the fence
of the current region to complete before writing and then set a new fence
after the drawing or dispatch commands which use the buffer.

#### Reading data

In the case of reading from the buffer, set a new fence after the drawing
or dispatch commands and then later wait for it to complete before reading the data.

!!! note
    Persistent mapped buffers in Orka are coherent. This means that
    writes by the GPU or CPU will be automatically visible to the other.
    There is no need to add a barrier before a fence is set in the case
    of reading from a mapped buffer.

### Unsynchronized mapped buffers

For use cases where a buffer does not need to be mapped indefinitely,
an unsynchronized mapped buffer can be created instead.
An unsynchronized mapped buffer can be mapped and then later unmapped.
This is useful so that data can be written to it by a non-rendering task.

To create an unsynchronized mapped buffer, use the function `Create_Buffer`
in package `:::ada Orka.Rendering.Buffers.Mapped.Unsynchronized`:

```ada
Buffer_5 : Unsynchronized_Mapped_Buffer := Create_Buffer
  (Kind   => Orka.Types.Int_Type,
   Length => 16,
   Mode   => Orka.Rendering.Buffers.Mapped.Write);
```

After the buffer has been created, it can be mapped:

```ada
Buffer_5.Map;
```

When the buffer is mapped, the procedures `Read_Data` or `Write_Data`
can be used to read or write data from or to the buffer, depending on
the used `Mode` in the `Create_Buffer` call.
These procedures can be called from any task.

After data has been read or written, the mapped buffer must be unmapped
again so that it can be used by the GPU:

```ada
Buffer_5.Unmap;
```

If the buffer is not mapped, the function `Buffer` can be called to
retrieve the actual `Buffer` object.
This object can then be used for other purposes, such as binding it
to some target so that it can be used by shaders.
See [Binding Buffers](#binding-buffers) on how to bind a `Buffer` object.

### Fences

A fence is needed when using persistent mapped buffers or when asynchronously
downloading data. The fence is retired after the previous rendering commands
have been completed.

To create a fence, call the function `Create_Buffer_Fence` in the package
`:::ada Orka.Rendering.Fences` and make sure the parameter `Regions` is equal
to the value used by `Create_Buffer` when creating the persistent mapped buffer:

```ada
Fence_1 : Buffer_Fence := Create_Buffer_Fence (Regions => 3);
```

It will actually create multiple fences, one for each region of the buffer.
One `Buffer_Fence` (with multiple regions) is sufficient for multiple
persistent mapped buffers as long as the buffers all have the same number of
regions and are all moved to the next region before the fence is moved to the
next region.

To wait for the fence of current region to retire, call procedure `Prepare_Index`:

```ada
declare
   Status : Fence_Status;
begin
   Fence_1.Prepare_Index (Status);
end;
```

This call may be done once at the start of a frame.
The `Status` will be `Signaled` if the fence was retired during or before the call.
If waiting failed or timed out, then the value will be `Not_Signaled`.

At the end of a frame, the procedure `Advance_Index` must be called to move the
fence to the next region:

```ada
Fence_1.Advance_Index;
```

## Barriers

Barrier must be inserted between compute and rendering commands to make
sure that the data becomes visible to a shader program or the video driver.

For example, if data was uploaded to a buffer or written by some compute
shader, and is then subsequently read as an SSBO by another program running
on the GPU, a `Shader_Storage` memory barrier must be inserted between the
two commands:

```ada
GL.Barriers.Memory_Barrier ((Shader_Storage => True, others => False));

--  Issue a rendering command in which a shader accesses the buffer as an SSBO
```

In another example, if a compute shader has written data to a buffer and
the buffer must then be copied to another buffer, insert a `Buffer_Update`
barrier:

```ada
GL.Barriers.Memory_Barrier ((Buffer_Update => True, others => False));

Buffer_1.Copy_Data (Buffer_2);
```

For use cases such as deferred shading where ordering of reads and writes
matters only to fragment shaders, the procedure `Memory_Barrier_By_Region`
can be used instead. In this case both `By_Region` and the requested barrier
must be set to `True`:

```ada
GL.Barriers.Memory_Barrier_By_Region
  ((By_Region => True, Texture_Fetch => True, others => False));
```

It is not harmful to use `Memory_Barrier` other than that
`Memory_Barrier_By_Region` may provide better performance.

The following barriers can be inserted:

| Name                  | Usable with `By_Region` | Usage                             |
|-----------------------|-------------------------|-----------------------------------|
| `Uniform`             | Yes                     | UBOs                              |
| `Texture_Fetch`       | Yes                     | `texture*()`                      |
| `Shader_Image_Access` | Yes                     | `image*()`                        |
| `Framebuffer`         | Yes                     | Textures attached to framebuffers |
| `Shader_Storage`      | Yes                     | SSBOs                             |
| `Element_Array`       | No                      | Buffers binded to `Index`         |
| `Command`             | No                      | Buffers binded to `Draw_Indirect` |
| `Pixel_Buffer`        | No                      | Buffers binded to `Pixel_Pack` or `Pixel_Unpack` |
| `Texture_Update`      | No                      | Textures                          |
| `Buffer_Update`       | No                      | Buffers and mapped buffers        |
| `Query_Buffer`        | No                      | Buffers binded to `Query`         |

!!! summary
    The kind of barrier that is needed depends on how the buffer is
    subsequently *read*.

*[SSBO]: Shader Storage Buffer Object
*[UBO]: Uniform Buffer Object
*[TBO]: Texture Buffer Object

  [url-memory-qlf]: https://www.khronos.org/opengl/wiki/Type_Qualifier_(GLSL)#Memory_qualifiers
  [url-memory-layout]: https://www.khronos.org/opengl/wiki/Block_Layout_Query#Memory_layout
  [url-ssbo-atomics]: https://www.khronos.org/opengl/wiki/Shader_Storage_Buffer_Object#Atomic_operations
  [url-github-pbo]: https://github.com/onox/orka/issues/32
