# General-purpose computing on the GPU

Besides rendering, Orka can also be used for general purpose computing
on the GPU (GPGPU) by using compute shaders. With compute shaders,
a framebuffer is not needed; compute shaders can read from and write to
buffers and/or images (textures) that have been binded.

A shader program that
uses a vertex and fragment shader requires a framebuffer.
The default framebuffer is not available unless a window has been created.
If the default framebuffer is not to be used, a new framebuffer object must
be created and binded instead. This is useful, for example, to attach a
texture to the framebuffer object and then later save the texture in a
[KTX](/resources/loaders/#ktx) file.

## Surfaceless context

To run compute shaders, a context needs to be created first.
If no default framebuffer is needed because there is nothing that needs
to be displayed in a window, a surfaceless context can be
created instead. This avoids needing a connection to a windowing system
such as a Wayland compositor.

To create a surfaceless context, create a context via EGL using the 'device'
platform by calling the function `Create_Context` in package
`:::ada Orka.Contexts.EGL`.
The function has a parameter `Device` that can be used to specify the device
that should be used in case your system has multiple GPUs.
See [EGL](/contexts/#egl) for more information.

## Buffers

Compute shaders often read from or write to buffers.
To make a buffer available, bind it as an SSBO to the index of a binding point.
See [SSBO](/rendering/buffers/#ssbo) on how to bind a buffer.

## Work groups

When running a compute shader on the GPU, work is divided over multiple threads.
The threads are grouped in what is called a workgroup.
Both the threads inside a workgroup and the workgroups themselves can be placed
on one to three axes.
The number of axes is arbitrary and depends on what is considered useful.
For example, a compute shader that applies element-wise transformations to a tensor
may use a single axis for both the threads and the workgroups,
while a shader which applies a matrix multiplication on two tensors may use two axes
for both the threads and the workgroups.

A shader can define a fixed number of threads per axis for a workgroups as following:

```glsl
layout(local_size_x = 256) in;
```

This should be a power of two, such as 64 or 256.
The reason for this is that threads inside a workgroup are grouped in smaller
groups called subgroups. These threads are executed in lock step.
If the local size is not a multiple of the subgroup size, some threads will be
inactive, decreasing the occupancy of the shader.
Most discrete GPUs have a subgroup size of 32 threads.
The constant `gl_WorkGroupSize` contains a vector of the number of threads on each axis.
The variable `gl_WorkGroupID` contains a vector with the indices of a workgroup (one for each axis).

The identifier of a thread inside a workgroup is given by `gl_LocalInvocationID`
and the identifier of a thread among all workgroups is given by `gl_GlobalInvocationID`
(both are `:::glsl uvec3`).

If a video driver supports the extension `ARB_compute_variable_group_size` then
variable sized compute shaders may be enabled by declaring in the shader:

```glsl
#extension GL_ARB_compute_variable_group_size : require
```

If a compute shader has a variable size, it must be dispatched in a different way
than fixed size compute shaders (explained below) and it must declare the layout differently:

```glsl
layout(local_size_variable) in;
```

For a variable sized compute shader, the variable `gl_LocalGroupSizeARB` must be used
instead of the constant `gl_WorkGroupSize`.

### Shared data

The threads inside a single workgroup can communicate with each other using shared data.
Threads from different workgroups cannot communicate with each other because work
groups are scheduled independently. In worst case a GPU may process the workgroups
one workgroup at a time. The only way for the threads from different workgroups to
communicate is to write data to a buffer, insert a memory barrier after the program
has been completed, and then launch the program again so that the threads can read
the data from the previous launch. If multiple threads wish to write to the same offset
in a buffer or a shared variable, atomic functions like `atomicAdd` should be used.

To create shared data in a shader, use the keyword `shared`:

```glsl
shared uint data[gl_WorkGroupSize.x];
```

When the threads have written their data to a shared variable, they all must
execute a memory and computation barrier:

```glsl
memoryBarrierShared();
barrier();
```

After all threads have passed the barriers, they can read the shared data.
This process can be repeated using a `:::glsl for` loop.

The function `groupMemoryBarrier` can be used for memory transactions involving
all types of memory, including buffers, images, and shared variables.

!!! warning "All threads of a workgroup must execute the barriers"
    It is important to note that all threads of a workgroup must execute a barrier,
    otherwise the compute shader will hang. Thus the barriers must not be placed
    inside an `:::glsl if` block if the control flow is not uniform.

### Voting

Threads inside a single subgroup operate in lock step and can also exchange information
by voting. This requires the extension [`ARB_shader_group_vote`][url-vote].
The function `anyInvocationARB` returns true if and only if at
least one thread in the subgroup used `:::glsl true` as the input to the function.
The function `allInvocationsARB` returns true if all threads
used the value `:::glsl true`.
The function `allInvocationsEqualARB` returns true if all threads agreed
on the boolean value of the first parameter.

However, for these functions to be used effectively it is often important to know
the size of the subgroup. The size of a subgroup can be retrieved from the constant
`gl_SubGroupSizeARB` (a `:::glsl uint`) from the extension `ARB_shader_ballot`.

If this extension is not present, the previously mentioned functions may be useful
only to choose between executing a fast algorithm (which works only if certain
conditions are met) and a slower one (which works in all cases).

A second way to vote is the function `ballotARB` from the extension
[`ARB_shader_ballot`][url-ballot].
It returns a bitfield of the type `:::glsl uint64_t` with a bit set for each
thread that provided the value `:::glsl true` as the input to the function.
This function is an effective way to exchange a small amount of information
among threads of a subgroup.

!!! note
    See [Mesamatrix][url-mesamatrix] for a list of extensions supported by
    each video driver.

### Broadcasting

The extension `ARB_shader_ballot` provides two more functions that can be used
to broadcast a value to all threads inside a subgroup.
The function `readFirstInvocationARB` broadcasts and returns the given value
of the first active thread to all other active threads in the subgroup.
The function `readInvocationARB` has a second parameter containing the index of
the thread in the subgroup whose value must be broadcasted to all other threads.

### Limits

The hardware places several limits on the number of threads and workgroups.
A workgroup often has a maximum of no less than 1024 threads.
The exact number can be queried with the function `Compute_Work_Group_Size` of a
`Shader` object. It returns a `Dimension_Size_Array`, which is an array containing
three values for the axes `X`, `Y`, and `Z`.

A workgroup containing a large number of threads, decreases the amount of shared data
available per thread.

## Launching compute shaders

To launch a compute shader, first bind the necessary buffers and images, then
bind the program containing a compute shader with the procedure `Bind_Shaders`
of a `Context` object
(See [Using a program](/rendering/programs/#using-a-program)), and then dispatch it.

To dispatch a fixed size compute shader, execute the procedure `Dispatch_Compute`
from the package `:::ada GL.Compute`:

```ada
declare
   function Groups (Elements, Group_Size : Unsigned_32) return Unsigned_32 is
     (Elements / Group_Size
        + (if Elements mod Group_Size = 0 then 0 else 1));

   use all type Orka.Index_3D;

   Group_Size : Dimension_Size_Array := Pipeline_1 (Compute_Shader).Value.Compute_Work_Group_Size;
   Size_X     : Unsigned_32          := Unsigned_32 (Group_Size (X));
begin
   GL.Compute.Dispatch_Compute
     (X => Groups (Elements => Elements, Group_Size => Size_X));
end;
```

To dispatch a variable sized compute shader, run procedure `Dispatch_Compute_Group_Size`:

```ada
GL.Compute.Dispatch_Compute_Group_Size
  (Group_Size => (Integer_32 (Size_X), 1, 1),
   X          => Groups (Elements => Elements, Group_Size => Size_X));
```

!!! warning "Insert a barrier barrier if needed"
    If a buffer was previously modified in any way, make sure
    to insert a `Shader_Storage` memory barrier before launching a compute shader.

*[SSBO]: Shader Storage Buffer Object

  [url-ballot]: https://registry.khronos.org/OpenGL/extensions/ARB/ARB_shader_ballot.txt
  [url-vote]: https://registry.khronos.org/OpenGL/extensions/ARB/ARB_shader_group_vote.txt
  [url-mesamatrix]: https://mesamatrix.net/
