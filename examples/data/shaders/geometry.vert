// SPDX-License-Identifier: Apache-2.0
#version 420 core

#extension GL_ARB_shader_storage_buffer_object : require

struct Vertex {
    vec4 position;
    vec4 color_sides;
};

layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    Vertex in_vertices[];
};

out vec3 vColor;
out float vSides;

void main(void) {
    const Vertex data = in_vertices[gl_VertexID];

    gl_Position = data.position;
    vColor = data.color_sides.xyz;
    vSides = data.color_sides.w;
}
