#version 420 core

#extension GL_ARB_shader_storage_buffer_object : require

// SPDX-License-Identifier: MIT
//
// Copyright (c) 2019 Jonathan Dupuy
// Copyright (c) 2020 onox <denkpadje@gmail.com>
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

struct leb_Node {
    uint id;    // binary code
    int depth;  // subdivision depth
};

vec4[3] DecodeTriangleVertices(in const leb_Node node);
vec4 planeToSphere(const int lebID, const vec4 v);

layout(std430, binding = 1) readonly restrict buffer NodeBuffer {
    uint u_LebNodeBuffer[];
};

layout(std140, binding = 0) uniform MatrixBuffer {
    mat4 u_ViewMatrix;
    mat4 u_ProjMatrix;
};

uniform int u_LebID;

layout(location = 0) out vec2 o_TexCoord;
layout(location = 1) out vec4 o_WorldPos;

void main()
{
    const int lebID = u_LebID;

    uint nodeID = u_LebNodeBuffer[gl_InstanceID];
    leb_Node node = leb_Node(nodeID, findMSB(nodeID));
    vec4 triangleVertices[3] = DecodeTriangleVertices(node);

    vec2 trianglePositions[3] = vec2[3](
        triangleVertices[0].xy,
        triangleVertices[1].xy,
        triangleVertices[2].xy
    );

    // change winding depending on node level
    if ((node.depth & 1) == 0) {
        vec2 tmp1 = trianglePositions[0];

        trianglePositions[0] = trianglePositions[2];
        trianglePositions[2] = tmp1;
    }

    const vec2 position = trianglePositions[gl_VertexID % 3];
    const vec4 worldPosition = planeToSphere(lebID, vec4(position, 1.0f, 1.0f));

    gl_Position = u_ProjMatrix * (u_ViewMatrix * worldPosition);
    o_TexCoord = position;
    o_WorldPos = worldPosition;
}
