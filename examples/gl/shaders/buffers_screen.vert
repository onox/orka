// SPDX-License-Identifier: Apache-2.0
#version 150

in vec2 position;
in vec2 texcoord;

out vec2 Texcoord;

void main()
{
    Texcoord = texcoord;
    gl_Position = vec4(position, 0.0, 1.0);
}
