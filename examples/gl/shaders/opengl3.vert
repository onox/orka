// SPDX-License-Identifier: Apache-2.0
#version 420 core

layout(location = 0) in vec2 in_Position;
layout(location = 1) in vec3 in_Color;

out vec3 ex_Color;

void main(void) {
   gl_Position = vec4(in_Position, 0.0, 1.0);
   ex_Color = in_Color;
}
