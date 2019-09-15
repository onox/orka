// SPDX-License-Identifier: Apache-2.0
#version 150 core

layout(points) in;
layout(line_strip, max_vertices = 64) out;

in vec3 vColor[];
in float vSides[];

out vec3 fColor;

const float PI = 3.141592653589793;

void main() {
    fColor = vColor[0];

    for (int i = 0; i <= vSides[0]; i++) {
        float angle = PI * 2.0 / vSides[0] * i;
        vec4 offset = vec4(cos(angle) * 0.3, -sin(angle) * 0.3, 0.0, 0.0);
        gl_Position = gl_in[0].gl_Position + offset;

        EmitVertex();
    }

    EndPrimitive();
}
