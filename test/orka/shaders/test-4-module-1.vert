#version 330 core

in vec3 in_Position;
in uint in_InstanceID;
flat out uint ex_InstanceID;

void main(void) {
    gl_Position = vec4(in_Position, 1.0);
    ex_InstanceID = in_InstanceID;
}
