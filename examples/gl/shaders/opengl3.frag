// SPDX-License-Identifier: Apache-2.0
#version 140

in vec3 ex_Color;
out vec4 out_Color;

const float gamma = 2.2;
const vec3 inverse_gamma = vec3(1.0 / gamma);

void main(void) {
   out_Color = vec4(pow(ex_Color, inverse_gamma), 1.0);
}
