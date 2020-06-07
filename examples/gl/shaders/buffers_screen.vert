// SPDX-License-Identifier: Apache-2.0
#version 420 core

#extension GL_ARB_shader_storage_buffer_object : require

struct Vertex {
    vec2 position;
    vec2 texcoord;
};

layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    Vertex in_vertices[];
};

out vec2 Texcoord;

void main()
{
    const Vertex data = in_vertices[gl_VertexID];

    Texcoord = data.texcoord;
    gl_Position = vec4(data.position, 0.0, 1.0);
}
