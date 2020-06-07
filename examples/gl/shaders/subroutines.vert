// SPDX-License-Identifier: Apache-2.0
#version 420 core

#extension GL_ARB_shader_subroutine : require
#extension GL_ARB_shader_storage_buffer_object : require

struct Vertex {
    vec4 position;
    vec4 color;
};

layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    Vertex in_vertices[];
};

out vec3 ex_Color;

subroutine vec3 update_position(in vec3 position);

subroutine(update_position)
vec3 update_x(in vec3 position)
{
    position.x -= 0.4;
    position.y -= 0.4;
    return position;
}

subroutine(update_position)
vec3 update_y(in vec3 position)
{
    position.x += 0.4;
    position.y += 0.4;
    return position;
}

subroutine uniform update_position update_pos;

void main(void) {
    gl_Position = vec4(update_pos(in_vertices[gl_VertexID].position.xyz), 1.0);
    ex_Color = in_vertices[gl_VertexID].color.xyz;
}
