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

layout(std430, binding = 1) readonly restrict buffer instanceBuffer {
    mat4 in_worldTM[];
};

out vec3 ex_Color;

uniform mat4 view;
uniform mat4 proj;

void main(void) {
    const Vertex data = in_vertices[gl_VertexID];

    gl_Position = proj * view * in_worldTM[gl_InstanceID] * data.position;
    ex_Color = data.color.xyz;
}
