# Vertex formats

The vertex format describes the attributes of vertices. Usually a vertex
has attributes like the position vector, a normal vector (used for lighting),
and the normalized pixel coordinate of some texture.

Traditionally the vertex format is described in OpenGL by something called
a Vertex Array Object. The vertex format of the VAO needs to correspond
with the attributes in the vertex shader. Every time a different vertex
format is used (hopefully no more than a few), a VAO object needs to binded,
and then the buffers containing the vertex data needs to be binded to this
object.

This is all quite cumbersome and it is easier to just do programmable
vertex pulling in the shaders instead. This is more flexible and no slower
on modern GPUs. With vertex pulling we can completely forget about VAOs.

## Programmable vertex pulling

With fixed-function input assembly, you might define attributes in your
shader like this:

```glsl
layout(location = 0) in vec3 position;
layout(location = 1) in vec3 color;
```

With vertex pulling, the attributes are instead fetched from an SSBO:

```glsl
struct Vertex {
    vec4 position;
    vec4 color;
};

layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    Vertex in_vertices[];
};
```

Note that we are using `vec4` here because `vec3` should be avoided in
buffers because of alignment issues. In this example that means we are
wasting some bytes on padding, but with more complex vertex formats
data can be packed in such a way that no padding is needed.

The data can then be read as follows:

```glsl
out vec3 vs_Color;

void main(void) {
    gl_Position = vec4(in_vertices[gl_VertexID].position.xyz, 1.0);
    vs_Color = in_vertices[gl_VertexID].color.xyz;
}
```

To access data that differs per vertex, use `gl_VertexID`. If you have
data that differs per draw call (for example, a matrix that describes the
orientation and position in world space), use `gl_InstanceID`.

If you interleave the attributes of your vertices, you only need one buffer.
However, you can bind additional buffers and then read from those buffers
for only specific vertices.

## Binding vertex data

In order to pull data from the buffer, we need to create a buffer
containing the attributes of the vertices:

```ada
Vertices : constant Single_Array
  := (-0.5, -0.5, 0.0, 1.0,     1.0, 0.0, 0.0, 0.0,
       0.5, -0.5, 0.0, 1.0,     0.0, 1.0, 0.0, 0.0,
       0.0,  0.5, 0.0, 1.0,     0.0, 0.0, 1.0, 0.0);

Vertex_Buffer : constant Buffer := Create_Buffer ((others => False), Vertices);
```

and then bind it as an SSBO to index 0 of the binding point:

```ada
Vertex_Buffer.Bind (Shader_Storage, 0);
```

We can then later activate a shader program and execute it to draw (in this case)
a triangle:

```ada
Orka.Rendering.Drawing.Draw (Triangles, 0, 3);
```

## Avoiding padding

To avoid padding, you may need to put some of the components of attributes
in other vectors:

```glsl
struct Vertex {
    vec4 position;
    vec4 color_texcoord;
    // color is shared between position (w) and color_texcoord (xy)
};
```

In this case the X component of the color is stored in the W component
of the `position` vector and the Y and Z components are stored in the X
and Y components of `color_texcoord`. The UV coordinates are stored in
the last two components (Z and W) of `color_texcoord`.

## Reducing bandwidth

To increase the performance of your vertex shaders, you may want to reduce
the bandwidth needed to fetch the data of your vertices. This means you
should spend no more than 16 to 24 bytes per vertex.

One way to do this is to use 16-bit floating point arrays instead of the
common 32-bit. In Orka that means using a buffer of type `Half_Type` instead
of `Single_Type`. Package `Orka.Types` provides the function `Convert` to
convert a `Single_Array` to a `Half_Array` and vice versa.

For example, a vertex that consists of the following attributes:

- `vec3` position (as 3x 16-bit `Half_Type`)
- `vec3` normal   (as 3x 16-bit `Half_Type`)
- `vec2` uv       (as 2x 16-bit `Half_Type`)

requires a total of 16 bytes and can be stored in one `uvec4` vector:

```glsl
layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    uvec4 in_vertices[];
};
```

The data can then be pulled and unpacked with the GLSL function
`unpackHalf2x16()`:

```glsl
const uvec4 data = in_vertices[gl_VertexID];

const vec2 tmp = unpackHalf2x16(data.y);

const vec3 position = vec3(unpackHalf2x16(data.x), tmp.x);
const vec3 normal = vec3(tmp.y, unpackHalf2x16(data.z));
const vec2 uv = unpackHalf2x16(data.w);
```
