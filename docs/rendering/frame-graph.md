# Frame graph

A frame graph is a directed acyclic graph of render passes and
resources like textures.
A frame graph provides the following features:

- Implicit automatic resource management,
  taking care of switching framebuffers, programs, and binding
  textures, and possibly providing aliasing of resources.
- Loose coupling between systems. A function can return a
  frame graph, which can then be inserted in another frame
  graph.
- Automatic updating of the render state, clearing and invalidating
  framebuffers, and inserting memory barriers when needed.
- Resolving multisample textures.

!!! info
    The various objects described on this page are declared in
    the package `:::ada Orka.Frame_Graphs` and `:::ada Orka.Rendering.States`.

## Creating a frame graph

A frame graph can be created by declaring it:

```ada
Graph_Builder : aliased Orka.Frame_Graphs.Frame_Graph
  (Maximum_Passes    => 3,
   Maximum_Handles   => 10,
   Maximum_Resources => 10);
```

Discriminant `Maximum_Handles` refers to the maximum amount of outgoing and
incoming edges (writes and reads) from render passes.
Increase the limits if needed.

## Resources

Resources are objects like textures.
Type `Resource` describes a texture, its format, size, etc.
A resource can be read as a:

- Framebuffer attachment (if the resource is a depth or stencil texture)
- Texture (if read in the shader using the function `:::glsl texture`)
- Image (if read in the shader using the function `:::glsl imageLoad`)

A resource can be written by a render pass as a:

- Framebuffer attachment (including textures which can be used as a color buffer)
- Image (if written in the shader using the function `:::glsl imageStore`)

A resource can be created by declaring it:

```ada
Resource_1 : aliased constant Orka.Frame_Graphs.Resource :=
  (Name        => +"Resource 1",
   Description =>
     (Kind    => LE.Texture_2D_Multisample,
      Format  => GL.Low_Level.Enum.Pixels.R11F_G11F_B10F,
      Size    => (1280, 720, 1),
      Samples => 2,
      others  => <>),
   others  => <>);
```

The name must be no longer than 16 characters.

### Views

If a resource is layered (e.g. if it is a 3D texture, 1D array, 2D (multisampled) array,
or a cube map (array)) then one of its layers can be used as the input or output
of a render pass by creating an object of the type `Resource_View`.
For example, if `Resource_Cube` has the kind `Texture_Cube_Map`, then the face *-Z* can be
selected with:

```ada
Resource_Negative_Z : aliased constant Orka.Frame_Graphs.Resource_View :=
  (Object => Resource_Cube'Access,
   Layer  => 5);
```

with `Layer` being the zero-based index of the selected layer.

!!! note "A `Resource_View` cannot be used as the subject in the function `Add_Input_Output`"

## Render passes

A render pass is a program and the full render state which it needs.
To create a render pass, first declare the render state which the pass needs:

```ada
Default_State : constant Orka.Rendering.States.State := (others => <>);
```

And then call the function `Add_Pass`:

```ada
type Program_1_Callback is limited new Orka.Frame_Graphs.Program_Callback with null record;

overriding procedure Run (Object : Program_1_Callback) is
begin
   --  Make sure to activate the program
   Program_1.Use_Program;

   --  Set uniforms and issue rendering commands here
end Run;

Program_1_Object : aliased Program_1_Callback;

Pass_1 : Render_Pass'Class := Graph_Builder.Add_Pass
  ("Pass 1", Default_State, Program_1_Object'Unchecked_Access);
```

Usually you want to store the program and callback together in a limited record
and providing an access value to the enclosing record as a discriminant to the callback type.
See [Executing a render pass](/rendering/frame-graph/#executing-a-render-pass) for an example.

The name must be no longer than 16 characters and can be queried with the
function `Name`.

!!! tip "State for a fullscreen render pass"
    A render pass which performs post-processing on a single fullscreen triangle
    could use a state like the following:

    ```ada
    Fullscreen_State : constant Orka.Rendering.States.State :=
      (Depth_Func => GL.Types.Always, others => <>);
    ```

After the passes have been created, they can be connected with the
resources. In the following example, `Pass_1` is a regular forward
pass rendering geometry, using a color and a depth texture,
and `Pass_2` is a fullscreen post-processing pass.
Resource `Resource_2` has a `GL.Pixels.Depth_Component32F` format.
If a depth texture is written to by a render apss using the procedure
`Add_Output`, depth testing and writing is automatically enabled.
If it is only read by using `Add_Input` then depth testing is enabled,
but writing (updating) the depth buffer is disabled.

```ada
Pass_1.Add_Output (Resource_1, Framebuffer_Attachment, 0);
Pass_1.Add_Output (Resource_2, Framebuffer_Attachment, 1);

Pass_2.Add_Input (Resource_1, Texture_Read, 0);
Pass_2.Add_Output (Resource_3, Framebuffer_Attachment, 0);
```

### Ordering of writes to a resource

Only one render pass can write to a resource.
If multiple passes were allowed to write to a specific resource,
the results would depend on the ordering of the passes.
To still allow writing by multiple passes, the ordering of the
writes must be made explicit.
This is done by giving resources a version and allowing a pass to
write to a specific version of the resource.
For example, if render pass A and B
both want to write to resource X, pass A must have an outgoing edge
to X~1 (version 1) and B has an incoming edge from X~1 and an outgoing
edge to X~2 (representing version 2 of X).

The version of a resource can be incremented by calling the
procedure `Add_Input_Output`:

```ada
Pass_A.Add_Output (Resource_X1, Framebuffer_Attachment);

declare
   Resource_X2 : Resource :=
     Pass_B.Add_Input_Output (Resource_X1, Framebuffer_Attachment, 0);
begin
    --  Use Resource_X2
end;
```

## Saving the graph to a JSON file

The graph can be saved to a JSON file by calling procedure `Write_Graph`:

```ada
Graph_Builder.Write_Graph (Location_Graphs, "graph.json");
```

`Location_Graphs` must be a location object implementing the
interface `Writable_Location`.

## Presenting a resource

After the graph has been defined and the render passes and resources
are connected to each other, the graph can be rendered to the screen
by chosing the resource which must be presented.
The presentation of the resource on the screen can happen in one of various ways,
depending on the write mode and the format of the resource:

1. **Using** the default framebuffer. If the render pass writing to the
   resource writes to it as a `Framebuffer_Attachment` and writes to no
   other color renderable resources, then it will use the default framebuffer
   for this pass. This method gives the highest performance.
   ```mermaid
   graph LR
     P1[Pass 1] -->|Attachment| R1[Resource 1]
     R1 --> PR[Present]
   ```
   becomes:
   ```mermaid
   graph LR
     DF[Default framebuffer] --> R1[Resource 1]
   ```


2. **Blitting** to the default framebuffer. If the render pass writes to
   multiple resources and the presented resourse is color renderable, then
   it will use a framebuffer object for the pass and then blit it to the default
   framebuffer.
   ```mermaid
   graph LR
     P1[Pass 1] -->|Attachment| R1[Resource 1]
     P1[Pass 1] -->|Attachment| R2[Resource 2]
     R1 --> PR[Present]
   ```
   becomes:
   ```mermaid
   graph LR
     F1[Framebuffer 1] --> R1[Resource 1]
     F1 --> R2[Resource 2]
     DF[Default framebuffer] --> R3[Resource 3]
     F1 -->|Blit| DF
   ```

3. **Rendering** the texture of the resource to the default framebuffer.
   If the resource is written using mode `Image_Store` or if the resource
   has a depth or stencil format, then it will render the texture of the
   resource to the default framebuffer using an extra vertex and fragment shader.
   ```mermaid
   graph LR
     P1[Pass 1] -->|Image write| R1[Resource 1]
     R1 --> PR[Present]
   ```
   becomes:
   ```mermaid
   graph LR
     F1[Framebuffer 1] --> R1[Resource 1]
     DF[Default framebuffer] --> R2[Resource 2]
     R1 --> DF
   ```

The frame graph automatically chooses the correct way to present the
requested resource.

### Creating a renderable graph

To render a frame graph, a `Renderable_Graph` object must be created:

```ada
Graph : Orka.Frame_Graphs.Renderable_Graph
  (Maximum_Passes    => Graph_Builder.Maximum_Passes,
   Maximum_Resources => Graph_Builder.Maximum_Resources,
   Graph             => Graph_Builder'Access,
   Context           => Context'Access);
```

The discriminants `Maximum_Passes` and `Maximum_Resources` must have the
same values as the frame graph to which the discriminant `Graph` points.

The object `Graph` needs to be declared just once. It is not needed to
redeclare it every frame.

### Rendering a texture to a window

A resource can be presented by rendering it to a given window using the
procedure `Render`:

```ada
Graph.Render (Window, Resource_3, Location_Shaders);
```

The `Renderable_Graph` object will be (re-)initialized when needed.

Presenting the resource containing the depth buffer will show:

![Depth buffer](../images/frame-graph-depth.png)

### Rendering a texture without a window

To present a resource in a frame graph without a window, for example when
using a surfaceless context, call the function `Render`:

```ada
Result : constant Orka.Rendering.Textures.Texture :=
  Graph.Render (Context, Resource_3, Location_Shaders);
```

This texture may then be written to a KTX file
(see [Saving to a KTX file](/rendering/textures/#saving-to-a-ktx-file)),
for example, or used for other purposes.

### Logging and saving the graph

The `Renderable_Graph` can be logged using the logging system of Orka
by calling procedure `Log_Graph`:

```ada
Graph.Log_Graph (Window);
```

Just like a `Frame_Graph`, a `Renderable_Graph` object can be saved to a
JSON file as well by calling the procedure `Write_Graph`:

```ada
Graph.Write_Graph (Location_Graphs, "graph.json");
```

`Location_Graphs` must be a location object implementing the
interface `Writable_Location`.
Saving a `Renderable_Graph` object instead of a `Frame_Graph` will
show which render passes and textures will be used and which ones
will be culled.

!!! warning "The `Renderable_Graph` must have been initialized"
    To log a renderable graph, it must have presented a resource
    at least once in order to initialize various data structures.

## Creating a reusable frame graph

A frame graph can be created by a function and then returned, so that it
can be inserted into another frame graph. When creating a frame graph that
will be consumed by other code, it is important to mark which resources
must be imported and exported.

First create a function which creates a frame graph.
It is recommended to use a given texture description for the resources which
are going to be imported. This will give these resources the appropriate size
and format, among other parameters in the texture description.

```ada
function Create_Graph
  (Object       : My_Type;
   Color, Depth : Orka.Rendering.Textures.Texture_Description) return Orka.Frame_Graphs.Frame_Graph
is
   use Orka.Frame_Graphs;

   Graph : aliased Orka.Frame_Graphs.Frame_Graph
     (Maximum_Passes    => 1,
      Maximum_Handles   => 4,
      Maximum_Resources => 4);

   Default_State : constant Orka.Rendering.States.State := (others => <>);
```

Next create a render pass and the two resources which are going to be imported later:

```ada
State : constant Orka.Rendering.States.State := (Default_State with delta Depth_Func => GL.Types.Always);
Pass  : Render_Pass'Class := Graph.Add_Pass ("render pass", State, Object.Callback'Unchecked_Access);

Resource_Color_V1 : constant Resource :=
  (Name        => +"color",
   Description => Color,
   others      => <>);

Resource_Depth_V1 : constant Resource :=
  (Name        => +"depth",
   Description => Depth,
   others      => <>);
```

Add these two resources as inputs and outputs. For example, the render pass
may want to overwrite the color resource, but use the depth resource without
modifying it.

```ada
declare
   Resource_Color_V2 : constant Resource :=
     Pass.Add_Input_Output (Resource_Color_V1, Framebuffer_Attachment, 0);
begin
   Pass.Add_Input (Resource_Depth_V1, Framebuffer_Attachment, 1);
end;
```

The last thing that needs to be done before returning the graph is marking
the resources `Resource_Color_V1` and `Resource_Depth_V1` as resources which
will be imported, and the resources `Resource_Color_V2` and `Resource_Depth_V1`
as resources which will be exported. See below how to import and export resources.

In this example it is not needed to export the resource `Resource_Depth_V1`
as well (since it is not modified),
but it may simplify connecting a chain of graphs to each other if
each graph imports and exports one color and one depth resource.

### Exporting and importing resources

Resources with only outgoing edges can be marked as imports,
and resources which are not written by a render pass as a framebuffer
attachment or image can be marked as exports.

For example, a frame graph may import a color and a depth resource,
perform one or more render passes, and then output newer versions of
these two resources. To do so, the resources must first be added as
inputs and outputs of these render passes and then marked as imported
and exported resources:

```ada
Graph.Import ([Resource_Color_V1, Resource_Depth_V1]);
Graph.Export ([Resource_Color_V2, Resource_Depth_V2]);
```

After some resources have been marked as resources which will be
imported and exported, the graph can be returned by the function which
created the graph.

### Executing a render pass

In the example above the object of the type `My_Type` has a `Program`
and a `Callback`. The declaration of `My_Type` will look as follows:

```ada
type My_Type_Program_Callback (Data : not null access My_Type) is
  limited new Orka.Frame_Graphs.Program_Callback with null record;

overriding procedure Run (Object : My_Type_Program_Callback);

type My_Type is tagged limited record
   Program  : Orka.Rendering.Programs.Program;
   Callback : aliased My_Type_Program_Callback (My_Type'Access);
end record;
```

If needed `My_Type` may also contain uniforms or buffers.

In the body of procedure `Run` you should bind buffers and samplers, and
execute draw calls using procedures from package `:::ada Orka.Rendering.Drawing`
or dispatching compute shaders.

Before running all the render passes by presenting a resource, make sure to
set uniforms and store buffers or uploading data to these buffers.
To do this, you probably want to have a function `Create_My_Type` returning an
object of type `My_Type` and a procedure `Set_Data` which sets any uniforms if
there are any. The function `Create_My_Type` should explicitly create a program
using the function `Create_Program` from the package `:::ada Orka.Rendering.Programs`
and initialize `Callback` by using `others => <>`.

!!! warning "Bind buffers in the procedure `Run`, not any time sooner"

## Connecting a reusable frame graph

Given a frame graph containing one or more resources, other graphs can be connected to it.
First create one or more graphs that are going to be connected later,
giving it the descriptions of a color and a depth resource which are the outputs
of some render pass:

```ada
Module_A_Graph : constant Frame_Graph :=
  Modules_A.Create_Graph (Resource_1.Description, Resource_2.Description);

Module_B_Graph : constant Frame_Graph :=
  Modules_B.Create_Graph (Resource_1.Description, Resource_2.Description);
```

These graphs can then be connected to the main frame graph `Graph_Builder` by
calling the function `Connect`:

```ada
Result_A : constant Resource_Array := Graph_Builder.connect (Module_A_Graph, [Resource_1, Resource_2]);
Result_B : constant Resource_Array := Graph_Builder.connect (Module_B_Graph, Result_A);
```

In this case the function `Connect` will return the resources which were
exported by the given frame graphs.
If instead of appending a graph, you want to insert it between two sets of resources,
then use the procedure `Connect`, giving it a third parameter containing an array of resources
to which the exported resources of the given frame graph must be connected.

The exported color resource of the second graph, `Result_B (1)` can then be added
as an input of another render pass, or be rendered to the screen by presenting it.
