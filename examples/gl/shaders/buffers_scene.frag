// SPDX-License-Identifier: Apache-2.0
#version 420 core

in vec3 Color;
in vec2 Texcoord;

out vec4 outColor;

layout(binding = 0) uniform sampler2D tex;

void main()
{
    outColor = texture(tex, Texcoord) * vec4(Color, 1.0);
}
