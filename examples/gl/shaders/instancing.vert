// SPDX-License-Identifier: Apache-2.0
#version 330 core

in  vec3 in_Position;
in  vec3 in_Color;
in  mat4 in_Model;
out vec3 ex_Color;

uniform mat4 view;
uniform mat4 proj;

void main(void) {
   gl_Position = proj * view * in_Model * vec4(in_Position, 1.0);
   ex_Color = in_Color;
}
