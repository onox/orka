#version 330 core

#extension GL_ARB_explicit_uniform_location : require

layout(location = 0) in vec3 in_Position;
layout(location = 1) in vec3 in_Normal;
layout(location = 2) in vec2 in_UV;
layout(location = 3) in uint in_InstanceID;

layout(location = 4) uniform mat4 model;
layout(location = 5) uniform mat4 view;
layout(location = 6) uniform mat4 proj;

out vec4 var_n;
out vec4 var_p;

flat out uint var_InstanceID;

void main(void) {
    // Ideally pre-compute modelView and normalMatrix on CPU
    mat4 modelView = view * model;
    mat4 normalMatrix = transpose(inverse(modelView));

    // Compute position of vertex and light in camera space
    vec4 p = modelView * vec4(in_Position, 1.0);

    gl_Position = proj * p;

    var_n = normalize(normalMatrix * vec4(in_Normal, 1.0));
    var_p = p;

    var_InstanceID = in_InstanceID;
}
