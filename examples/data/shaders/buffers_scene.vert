// SPDX-License-Identifier: Apache-2.0
#version 420 core

#extension GL_ARB_shader_storage_buffer_object : require

struct Vertex {
    vec4 position;
    vec4 color_texcoord;
    // color is shared between position (w) and color_texcoord (xy)
};

layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    Vertex in_vertices[];
};

out vec3 Color;
out vec2 Texcoord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

uniform vec4 overrideColor;

void main()
{
    const Vertex data = in_vertices[gl_VertexID];

    Color = vec3(overrideColor) * vec3(data.position.w, data.color_texcoord.xy);
    Texcoord = data.color_texcoord.zw;
    gl_Position = proj * view * model * vec4(data.position.xyz, 1.0);
}
