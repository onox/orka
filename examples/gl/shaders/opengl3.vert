// SPDX-License-Identifier: Apache-2.0
#version 420 core

#extension GL_ARB_shader_storage_buffer_object : require

struct Vertex {
    vec4 position;
    vec4 color;
};

layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    Vertex in_vertices[];
};

out vec3 ex_Color;

void main(void) {
    gl_Position = in_vertices[gl_VertexID].position;
    ex_Color = in_vertices[gl_VertexID].color.xyz;
}
