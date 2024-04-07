#version 420 core

// SPDX-License-Identifier: MIT
//
// Copyright (c) 2019 Jonathan Dupuy
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

layout(triangles) in;
layout(triangle_strip, max_vertices = 3) out;

layout(location = 0) in vec2 i_TexCoord[];
layout(location = 1) in vec4 i_WorldPos[];

layout(location = 0) out vec2 o_TexCoord;
layout(location = 1) out vec4 o_WorldPos;
layout(location = 2) noperspective out vec3 o_Distance;

uniform vec4 u_ScreenResolution;
// Only using first vec2 (xy, containing width and height)

in gl_PerVertex
{
  vec4 gl_Position;
} gl_in[];

out gl_PerVertex
{
  vec4 gl_Position;
};

void main()
{
    vec2 p0 = u_ScreenResolution.xy * gl_in[0].gl_Position.xy / gl_in[0].gl_Position.w;
    vec2 p1 = u_ScreenResolution.xy * gl_in[1].gl_Position.xy / gl_in[1].gl_Position.w;
    vec2 p2 = u_ScreenResolution.xy * gl_in[2].gl_Position.xy / gl_in[2].gl_Position.w;
    vec2 v[3] = vec2[3](p2 - p1, p2 - p0, p1 - p0);
    float area = abs(v[1].x * v[2].y - v[1].y * v[2].x);

    for (int i = 0; i < 3; ++i) {
        o_TexCoord = i_TexCoord[i];
        o_WorldPos = i_WorldPos[i];
        o_Distance = vec3(0);
        o_Distance[i] = area * inversesqrt(dot(v[i],v[i]));
        gl_Position = gl_in[i].gl_Position;
        EmitVertex();
    }

    EndPrimitive();
}
