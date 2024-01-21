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

vec2 BarycentricInterpolation(in vec2 v[3], in vec2 u);
vec4 planeToSphere(const int lebID, vec2 vertex, out vec2 lonLatUV);
int leb_MaxDepth(const int lebID);

layout(vertices = 3) out;

flat in uint vs_NodeDepth[];

layout(std140, binding = 0) uniform MatrixBuffer {
    mat4 u_ModelMatrix;
    mat4 u_ModelViewMatrix;
    mat4 u_ProjMatrix;
};

uniform int u_LebID;
uniform int u_MeshletSubdivision;

const float E = 2.718281828459045;

const float MAX_DISTANCE = 512.0;

void main()
{
    if (gl_InvocationID == 0) {
        const vec2[3] triangleVertices = {
            gl_in[0].gl_Position.xy,
            gl_in[1].gl_Position.xy,
            gl_in[2].gl_Position.xy,
        };
        const vec2 position = BarycentricInterpolation(triangleVertices, vec2(0.5));

        vec2 lonLatUV;
        const vec4 eyespacePosition = u_ModelViewMatrix * planeToSphere(u_LebID, position, lonLatUV);

        int level = 1;

        if (vs_NodeDepth[gl_InvocationID] >= leb_MaxDepth(u_LebID)) {
            const float dist = length(eyespacePosition);
            const float distanceRatio = clamp(exp(-((100.0 / MAX_DISTANCE * dist + 10.0) / 10.0)) * E, 0.0, 1.0);

            level = int(round(mix(1.0, 64.0, distanceRatio)));
            level = min(1 << u_MeshletSubdivision, level);
        }

        int outer = level;
        int inner = level;

        gl_TessLevelOuter[0] = outer;
        gl_TessLevelOuter[1] = outer;
        gl_TessLevelOuter[2] = outer;

        gl_TessLevelInner[0] = inner;
    }

    gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;
}
