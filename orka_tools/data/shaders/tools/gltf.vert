#version 420 core

#extension GL_ARB_shader_draw_parameters       : require
#extension GL_ARB_shader_storage_buffer_object : require

//  SPDX-License-Identifier: Apache-2.0

layout(std430, binding = 0) readonly restrict buffer matrixBuffer {
    mat4 matrices[];
};

layout(std430, binding = 1) readonly restrict buffer vertexBuffer {
    uvec4 in_vertices[];
};

layout(binding = 0) uniform sampler2DArray diffuseTexture;

uniform mat4 view;
uniform mat4 proj;
uniform vec4 lightPosition;

out VS_OUT {
    vec3 N;
    vec3 L;
    vec3 V;
    vec2 uv;
} vs_out;

flat out uint var_InstanceID;

void main(void) {
    int instanceID = gl_BaseInstanceARB + gl_InstanceID;

    // Ideally pre-compute modelView and normalMatrix on CPU
    mat4 world = matrices[instanceID];

    mat4 modelView = view * world;
    mat4 normalMatrix = transpose(inverse(modelView));

    const uvec4 data = in_vertices[gl_VertexID];

    const vec2 tmp = unpackHalf2x16(data.y);
    const vec3 in_Position = vec3(unpackHalf2x16(data.x), tmp.x);
    const vec3 in_Normal = vec3(tmp.y, unpackHalf2x16(data.z));
    const vec2 in_UV = unpackHalf2x16(data.w);

    // Compute position of vertex and light in camera space
    vec4 p = modelView * vec4(in_Position, 1.0);

    gl_Position = proj * p;

    vs_out.N = mat3(normalMatrix) * in_Normal;
    vs_out.L = (view * lightPosition).xyz - p.xyz;
    vs_out.V = -p.xyz;

    vs_out.uv = in_UV;

    // Modulo number of textures
    const int layers = textureSize(diffuseTexture, 0).z;
    var_InstanceID = uint(mod(instanceID, layers));
}
