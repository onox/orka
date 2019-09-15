// SPDX-License-Identifier: Apache-2.0
#version 150 core

in vec2 position;
in vec3 color;
in float sides;

out vec3 vColor;
out float vSides;

void main(void) {
   gl_Position = vec4(position, 0.0, 1.0);
   vColor = color;
   vSides = sides;
}
