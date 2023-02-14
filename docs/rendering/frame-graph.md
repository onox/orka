# Frame graph

A frame graph is a directed acyclic graph of render passes and
resources like textures and buffers.
A frame graph provides the following features:

- Implicit automatic resource management,
  taking care of switching framebuffers, programs, and binding
  textures and buffers, and possibly providing aliasing of resources.
- Loose coupling between systems. A function could return a
  whole frame graph, which can then be inserted in another frame
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
Graph_Builder : Orka.Frame_Graphs.Frame_Graph
  (Maximum_Passes    => 3,
   Maximum_Handles   => 10,
   Maximum_Resources => 10);
```

Discriminant `Maximum_Handles` refers to the maximum amount of outgoing and
incoming edges (writes and reads) from render passes.
Increase the limits if needed.

## Resources

Resources are objects like textures or buffers.
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
Resource_1 : Resource :=
  (Name    => +"Resource 1",
   Kind    => LE.Texture_2D_Multisample,
   Format  => GL.Low_Level.Enum.Pixels.R11F_G11F_B10F,
   Size    => (1280, 720, 1),
   Samples => 2,
   others  => <>);
```

The name must be no longer than 16 characters.

!!! bug "Only resources are currently supported"

## Render passes

A render pass is a program and the full render state which it needs.
To create a render pass, first declare the render state which the pass needs:

```ada
Default_State : constant Orka.Rendering.States.State := (others => <>);
```

And then call the function `Add_Pass`:

```ada
procedure Run_Program_1 (P : in out Program) is
begin
   --  Set uniforms and issue rendering commands here
end Run_Program_1;

Pass_1 : Render_Pass'Class := Graph_Builder.Add_Pass
  ("Pass 1", Default_State, Program_1, Run_Program_1'Unrestricted_Access);
```

The name must be no longer than 16 characters and can be queried with the
function `Name`.

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

## Presenting a resource

After the graph has been defined and the render passes and resources
are connected to each other, the graph can be rendered to the screen
by chosing the resource which must be presented.
The presenting the resource on the screen can happen in one of various ways,
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
requested resource. To choose a resource to present, the graph must be
culled by calling the function `Cull`, which returns a `Renderable_Graph` object:

```ada
Graph : Renderable_Graph'Class := Graph_Builder.Cull (Resource_3);
```

The `Renderable_Graph` object must subsequently be initialized once:

```ada
Graph.Initialize (Location_Shaders, FB_D);
```

Variable `FB_D` is the default framebuffer.
See [Creating a framebuffer](/rendering/framebuffers/#creating-a-framebuffer)
for more information on how to create the default framebuffer.

After `Graph` has been initialized, it can present the resource to the
screen by calling procedure `Render`:

```ada
Graph.Render (Context);
```

Presenting the resource containing the depth buffer will show:

![Depth buffer](../images/frame-graph-depth.png)

### Logging and saving the graph

The `Renderable_Graph` can be logged using the logging system of Orka
by calling procedure `Log_Graph`:

```ada
Graph.Log_Graph;
```

The graph can be saved to a JSON file by calling procedure `Write_Graph`:

```ada
Graph.Write_Graph (Location_Graphs, "graph.json");
```

`Location_Graphs` must be a location object implementing the
interface `Writable_Location`.

## Exporting and importing resources

Resources with only outgoing edges can be marked as imports,
and resources with only incoming edges can be marked as exports.

The imported and exported resources can be queried so that a frame graph
can be inserted into a bigger frame graph.

!!! bug "Not yet implemented"
