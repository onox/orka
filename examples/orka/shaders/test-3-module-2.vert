// SPDX-License-Identifier: Apache-2.0
#version 330 core

in vec3 in_Position;

void main(void) {
    gl_Position = vec4(in_Position, 1.0);
}
