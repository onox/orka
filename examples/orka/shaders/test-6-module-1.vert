#version 330 core

#extension GL_ARB_shader_draw_parameters : require

layout(location = 0) in vec3 in_Position;
layout(location = 1) in vec3 in_Normal;
layout(location = 2) in vec2 in_UV;

uniform samplerBuffer matrixBuffer;
uniform int indexOffset;
uniform mat4 view;
uniform mat4 proj;

out VS_OUT {
    vec3 N;
    vec3 L;
    vec3 V;
    vec2 uv;
} vs_out;

flat out uint var_InstanceID;

mat4 get_matrix(samplerBuffer buffer, int index) {
    int offset = index * 4;
    vec4 v1 = texelFetch(buffer, offset + 0);
    vec4 v2 = texelFetch(buffer, offset + 1);
    vec4 v3 = texelFetch(buffer, offset + 2);
    vec4 v4 = texelFetch(buffer, offset + 3);
    return mat4(v1, v2, v3, v4);
}

void main(void) {
    int instanceID = gl_DrawIDARB;

    // Ideally pre-compute modelView and normalMatrix on CPU
    mat4 world = get_matrix(matrixBuffer, indexOffset + instanceID);
    mat4 modelView = view * world;
    mat4 normalMatrix = transpose(inverse(modelView));

    // Compute position of vertex and light in camera space
    vec4 p = modelView * vec4(in_Position, 1.0);

    gl_Position = proj * p;

    vs_out.N = mat3(normalMatrix) * in_Normal;
    vs_out.L = (view * vec4(0.0, 0.0, 0.0, 1.0)).xyz - p.xyz;
    vs_out.V = -p.xyz;

    vs_out.uv = in_UV;

    // Modulo number of textures
    var_InstanceID = uint(mod(instanceID, 7.0));
}
