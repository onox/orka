# General-purpose computing on the GPU

Besides rendering, Orka can also be used for general purpose computing
on the GPU (GPGPU) by using compute shaders. With compute shaders,
a framebuffer is not needed; compute shaders can read from and write to any
buffers and/or images (textures) that are binded.

A shader program that
uses a vertex and fragment shader does require a framebuffer.
The default framebuffer is not available unless a window has been created.
If the default framebuffer cannot be used, a new framebuffer object must be
created and binded instead.

## Algorithms

### Prefix sum

!!! note "TODO"

### Fast Fourier Transform (FFT)

!!! note "TODO"
