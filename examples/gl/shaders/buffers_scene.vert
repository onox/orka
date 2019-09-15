// SPDX-License-Identifier: Apache-2.0
#version 150

in vec3 position;
in vec3 color;
in vec2 texcoord;

out vec3 Color;
out vec2 Texcoord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

uniform vec4 overrideColor;

void main()
{
    Color = vec3(overrideColor) * color;
    Texcoord = texcoord;
    gl_Position = proj * view * model * vec4(position, 1.0);
}
