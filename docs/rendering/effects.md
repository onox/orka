# Effects

Orka provides a few packages which can be used for post-processing of textures.

!!! info
    The various objects described on this page are declared in
    the package `:::ada Orka.Rendering.Effects` and its child packages.

## Filters

The package `:::ada Orka.Effects.Filters` provides the following filters:

- Separable filter. Needs a `Float_32_Array` representing the kernel.
  A function which returns a Gaussian kernel is provided.
- Moving average filter

A Gaussian blur using the separable filter gives better performance
than the moving average filter for small kernels (weak blurs).
However, the moving average uses a computer shader with a O(1)
time complexity and provides a consistent performance independent of the
blur radius.

### Separable filter

The type `Separable_Filter` can be used to apply a Gaussian blur to a texture.
First create a kernel and then the separable filter by calling the function `Create_Filter`:

```ada
Kernel   : Float_32_Array   := Gaussian_Kernel (Radius => 24);
Filter_1 : Separable_Filter := Create_Filter (Location, Texture_1, Kernel);
```

The `Kernel` must consist of a sequence of offsets, followed by a
sequence of weights. The function `Gaussian_Kernel` takes care of this.
Good values for `Radius` for weak, normal, or strong blurs are 6, 24, and 48.

The filter can then be applied to the texture:

```ada
Filter_1.Render (Passes => 1);
```

For better performance, it is recommended to first downsample a
texture to half the size (with procedure `Resolve_To` in the package
`:::ada Orka.Rendering.Framebuffers`) before applying the filter to it. However,
this can visibly reduce the quality of the image for very small kernels.

### Moving average filter

The type `Moving_Average_Filter` provides a filter that computes the moving
average per row in a compute shader for a O(1) time complexity.
This gives it a consistent performance which does not depend on the blur radius.
The filter does not require a kernel and can be created immediately by
calling the function `Create_Filter`:

```ada
Filter_2 : Moving_Average_Filter := Create_Filter (Location, Texture_1, Radius => 4);
```

Good values for `Radius` for weak, normal, or strong blurs are 1, 4, and 8.
After the filter has been created, it can be applied to the texture:

```ada
Filter_2.Render (Passes => 2);
```

With a single pass the moving average filter acts as a box blur.
The filters should be applied multiple times so that it approximates a Gaussian blur.
By default it is applied in two passes.
