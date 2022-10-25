// SPDX-License-Identifier: Apache-2.0
#version 420 core

#extension GL_ARB_shader_storage_buffer_object : require

layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    vec2 in_vertices[];
};

void main(void) {
    gl_Position = vec4(in_vertices[gl_VertexID], 0.0, 1.0);
}
