# General-purpose computing on the GPU

Besides rendering, Orka can also be used for general purpose computing
on the GPU (GPGPU) by using compute shaders. With compute shaders,
a framebuffer is not needed; compute shaders can read from and write to any
buffers and/or images (textures) that are binded.

A shader program that
uses a vertex and fragment shader does require a framebuffer.
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

## Algorithms

### Prefix sum

!!! note "TODO"

### Fast Fourier Transform (FFT)

!!! note "TODO"
