// SPDX-License-Identifier: Apache-2.0
#version 420 core

in vec3 ex_Color;

out vec4 out_Color;

void main(void) {
   out_Color = vec4(ex_Color, 1.0);
}
