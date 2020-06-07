# Buffers

Buffers are objects that contain data that can be read or written by
shaders on the GPU. Buffers must contain homogeneous data (all elements
in the buffer have the same type) and can be numeric or composite. Composite
data can be vectors, matrices, or draw commands for indirect drawing.

!!! info
    The various objects described on this page are declared in
    the package `Orka.Rendering.Buffers` and its child packages.

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
Indices  : UInt_Array := (1, 2, 0, 0, 2, 3);

Buffer_2 : Buffer := Create_Buffer ((others => False), Indices);
```

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
GL.Barriers.Memory_Barrier
  ((By_Region => False, Buffer_Update => True, others => False));
```

and then call procedure `Get_Data`:

```ada
declare
   Data : Single_Array (1 .. 16) := (others => 0.0);
begin
   Buffer_0.Get_Data (Data);
end;
```

This procedure may stall the CPU.

!!! note "TODO Asynchronously downloading data"

## Clearing data

Regardless of the value of `Dynamic_Storage`, data from a buffer can be
cleared with the procedure `Clear_Data`:

```ada
declare
   Data : UInt_Array := (1, 2, 0);
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

Buffer objects implement the `Bindable_Buffer` interface, which provides
the procedure `Bind`. This procedure can be used to bind the buffer to a
target so that it can be used by certain operations like indirect drawing.
Valid targets are:

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

* `Atomic_Counter`
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
   Data : Int_Array (1 .. 16) := (others => 0);
begin
   Buffer_4.Read_Data (Data);
end;
```

Data of the following types from package `GL.Types` can be read or written:

* `UByte_Array`
* `UShort_Array`
* `UInt_Array`
* `Byte_Array`
* `Short_Array`
* `Int_Array`
* `Half_Array`
* `Single_Array`
* `Double_Array`

of the following types from package `GL.Types.Indirect`:

* `Arrays_Indirect_Command_Array`
* `Elements_Indirect_Command_Array`
* `Dispatch_Indirect_Command_Array`

and of the following types from `Orka.Types.Singles` and `Orka.Types.Doubles`:

* `Vector4_Array`
* `Matrix_Array`

Additionally, the non-array versions of the last five types can also be written.

### Persistent mapped buffers

Persistent mapped buffers are buffers that are mapped when they are
created and then never unmapped (until the buffer is deleted). This
kind of mapped buffer is useful for data that is updated every frame.

Because the mapping is persistent, the GPU may read from or write to
the buffer while it is mapped. To guarantee mutually exclusive access
between the GPU and the CPU, the buffer must be split into multiple
regions and fences must be used to make sure the GPU and CPU never
operate on the same region.

If it is intended to write to the buffer, then you must wait for the fence
of the current region to complete before writing and then set a new fence
after the drawing or dispatch commands which use the buffer.
In the case of reading from the buffer, set a new fence after the drawing
or dispatch commands and wait for it to complete before reading the data.

!!! note
    Persistent mapped buffers in Orka are coherent. This means that
    writes by the GPU or CPU will be automatically visible to the other.

To create a persistent mapped buffer, first instantiate
generic package `Orka.Rendering.Buffers.Mapped.Persistent`:

```ada
type Region_Type is mod 3;

package PMB is new Orka.Rendering.Buffers.Mapped.Persistent (Region_Type);
```

and then call function `Create_Buffer`:

```ada
Buffer_4 : PMB.Persistent_Mapped_Buffer := PMB.Create_Buffer
  (Kind   => Orka.Types.Int_Type,
   Length => 16,
   Mode   => Orka.Rendering.Buffers.Mapped.Read);
```

At the end of a frame, the buffer can be set to the next region:

```ada
Buffer_4.Advance_Index;
```

Note that this procedure does not set or wait for any fence.
See [Fences](#fences) on how to set and wait for a fence.

### Unsynchronized mapped buffers

!!! note "TODO"

### Fences

```ada
type Region_Type is mod 3;

package Fences is new Orka.Rendering.Fences (Region_Type);
```

## Barriers

!!! note "TODO"

```ada
GL.Barriers.Memory_Barrier
  ((By_Region => False, Shader_Storage => True, others => False));
```

  [url-memory-qlf]: https://www.khronos.org/opengl/wiki/Type_Qualifier_(GLSL)#Memory_qualifiers
  [url-memory-layout]: https://www.khronos.org/opengl/wiki/Block_Layout_Query#Memory_layout
  [url-ssbo-atomics]: https://www.khronos.org/opengl/wiki/Shader_Storage_Buffer_Object#Atomic_operations
