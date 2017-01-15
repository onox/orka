#version 330 core

#extension GL_ARB_explicit_uniform_location : require

layout(location = 0) in vec3 in_Position;
layout(location = 1) in vec3 in_Normal;
layout(location = 2) in vec2 in_UV;
layout(location = 3) in uint in_InstanceID;

layout(location = 4) uniform samplerBuffer matrixBuffer;
layout(location = 5) uniform mat4 view;
layout(location = 6) uniform mat4 proj;

out vec4 var_n;
out vec4 var_p;
out vec2 var_uv;

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
    // Ideally pre-compute modelView and normalMatrix on CPU
    mat4 world = get_matrix(matrixBuffer, int(in_InstanceID));
    mat4 modelView = view * world;
    mat4 normalMatrix = transpose(inverse(modelView));

    // Compute position of vertex and light in camera space
    vec4 p = modelView * vec4(in_Position, 1.0);

    gl_Position = proj * p;

    var_n = normalize(normalMatrix * vec4(in_Normal, 1.0));
    var_p = p;

    var_uv = in_UV;

    // Modulo number of textures
    var_InstanceID = uint(mod(in_InstanceID, 7.0));
}
