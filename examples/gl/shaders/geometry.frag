// SPDX-License-Identifier: Apache-2.0
#version 150 core

in vec3 fColor;

out vec4 outColor;

void main(void) {
   outColor = vec4(fColor, 1.0);
}
