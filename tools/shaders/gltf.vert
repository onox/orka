#version 330 core

#extension GL_ARB_shader_draw_parameters : require
#extension GL_ARB_shader_storage_buffer_object : require

layout(location = 0) in vec3 in_Position;
layout(location = 1) in vec3 in_Normal;
layout(location = 2) in vec2 in_UV;

layout(std430, binding = 0) readonly restrict buffer matrixBuffer {
    mat4 matrices[];
};

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
    int instanceID = gl_DrawIDARB;

    // Ideally pre-compute modelView and normalMatrix on CPU
    mat4 world = matrices[instanceID];

    mat4 modelView = view * world;
    mat4 normalMatrix = transpose(inverse(modelView));

    // Compute position of vertex and light in camera space
    vec4 p = modelView * vec4(in_Position, 1.0);

    gl_Position = proj * p;

    vs_out.N = mat3(normalMatrix) * in_Normal;
    vs_out.L = (view * lightPosition).xyz - p.xyz;
    vs_out.V = -p.xyz;

    vs_out.uv = in_UV;

    // Modulo number of textures
    var_InstanceID = uint(mod(instanceID, 7.0));
}
