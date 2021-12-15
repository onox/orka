// SPDX-License-Identifier: Apache-2.0
#version 420 core

#extension GL_ARB_shader_draw_parameters       : require
#extension GL_ARB_shader_storage_buffer_object : require

struct Vertex {
    vec2 position;
};

layout(std430, binding = 0) readonly restrict buffer vertexBuffer {
    Vertex in_vertices[];
};

uniform int mode;

flat out uint ex_InstanceID;

void main(void) {
    int instanceID;

    switch (mode) {
        case 0:
            instanceID = gl_DrawIDARB;
            break;
        case 1:
            instanceID = gl_InstanceID;
            break;
        case 2:
            instanceID = gl_BaseInstanceARB + gl_InstanceID;
            break;
        default:
            instanceID = 0;
    };

    gl_Position = vec4(0.5 * in_vertices[gl_VertexID].position, 0.0, 1.0);
    gl_Position.x += 0.5 * gl_InstanceID - 0.5;
    gl_Position.y *= -1;
    ex_InstanceID = instanceID;
}
