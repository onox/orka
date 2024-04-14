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
Filter_1 : Separable_Filter := Create_Filter (Context, Location, Texture_1, Kernel);
```

The `Kernel` must consist of a sequence of offsets, followed by a
sequence of weights. The function `Gaussian_Kernel` takes care of this.
Good values for `Radius` for weak, normal, or strong blurs are 6, 24, and 48.

#### Rendering

To apply the filter to a texture, create a frame graph with the function `Create_Graph`
and connect it to your main frame graph:

```ada
Filter_Graph : constant Orka.Frame_Graphs.Frame_Graph :=
  Filter_1.Create_Graph (Resource_Color.Description, Passes => 1);

Resources_Filter : constant Orka.Frame_Graphs.Resource_Array :=
  Main_Graph.Connect (Filter_Graph, [Resource_Color]);
```

The variable `Resources_Filter` contains one resource to which yet another frame graph can be connected.
Finally, create a `Renderable_Graph` and call its procedure `Render` to render the whole frame graph.

See [Frame graph](/rendering/frame-graph/) for more information on how to
build and render a frame graph.

For better performance, it is recommended to first downsample a
texture to half the size somewhere in the frame graph before applying the
filter to it.
However, this can visibly reduce the quality of the image for very small kernels.

### Moving average filter

The type `Moving_Average_Filter` provides a filter that computes the moving
average per row in a compute shader for a O(1) time complexity.
This gives it a consistent performance which does not depend on the blur radius.
The filter does not require a kernel and can be created immediately by
calling the function `Create_Filter`:

```ada
Filter_2 : Moving_Average_Filter := Create_Filter (Context, Location, Texture_1, Radius => 4);
```

Good values for `Radius` for weak, normal, or strong blurs are 1, 4, and 8.

#### Rendering

After the filter has been created, it can be applied to the texture:

```ada
Filter_Graph : constant Orka.Frame_Graphs.Frame_Graph :=
  Filter_2.Create_Graph (Resource_Color.Description, Passes => 2);
```

With a single pass the moving average filter acts as a box blur.
The filters should be applied multiple times so that it approximates a Gaussian blur.
By default it is applied in two passes.
