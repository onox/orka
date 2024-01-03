#version 420 core

// SPDX-License-Identifier: MIT
//
// Copyright (c) 2024 onox <denkpadje@gmail.com>
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

vec4 planeToSphere(const int lebID, const vec2 v);

layout(std140, binding = 0) uniform MatrixBuffer {
    mat4 u_ModelMatrix;
    mat4 u_ViewMatrix;
    mat4 u_ProjMatrix;
};

layout(triangles, equal_spacing, ccw) in;

layout(location = 0) out vec2 o_TexCoord;
layout(location = 1) out vec4 o_WorldPos;

uniform int u_LebID;

void main()
{
    // Barycentric interpolation
    const vec2 position =
        gl_in[0].gl_Position.xy * gl_TessCoord.x +
        gl_in[1].gl_Position.xy * gl_TessCoord.y +
        gl_in[2].gl_Position.xy * gl_TessCoord.z;

    const vec4 worldPosition = planeToSphere(u_LebID, position);

    gl_Position = u_ProjMatrix * (u_ViewMatrix * (u_ModelMatrix * worldPosition));
    o_TexCoord = position;
    o_WorldPos = worldPosition;
}
