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

// Contributions:
//
//   * Create a static meshlet on the GPU in function GetMeshletVertex
//     instead of using a VBO created on the CPU with a C implementation
//     of the LEB library.
//
//   * Do barycentric interpolation before multiplying with view and
//     projection matrices to avoid low-poly appearance of planet at
//     higher meshlet subdivision levels.

struct leb_Node {
    uint id;    // binary code
    int depth;  // subdivision depth
};

vec4[3] DecodeTriangleVertices(in const leb_Node node);
vec2 BarycentricInterpolation(in vec2 v[3], in vec2 u);
vec4 planeToSphere(const int lebID, const vec4 v);

layout(std430, binding = 1) readonly restrict buffer NodeBuffer {
    uint u_LebNodeBuffer[];
};

layout(std140, binding = 0) uniform MatrixBuffer {
    mat4 u_ViewMatrix;
    mat4 u_ProjMatrix;
};

uniform int u_LebID;
uniform int u_MeshletSubdivision;

mat2x3 leb_DecodeNodeAttributeArray(in const leb_Node node, in const mat2x3 data);

vec2 DecodeMeshletVertices(in const leb_Node node, in const int vertexID)
{
    const vec3 xPos = vec3(0, 0, 1), yPos = vec3(1, 0, 0);
    mat2x3 pos = leb_DecodeNodeAttributeArray(node, mat2x3(xPos, yPos));
    return vec2(pos[0][vertexID], pos[1][vertexID]);
}

vec2 GetMeshletVertex(int vertexID)
{
    const uint meshletIndexCount = 3 << (2 * u_MeshletSubdivision);

    const int staticDepth = int(log2(meshletIndexCount / 3));
    const uint staticMask = 1u << staticDepth + 1; // for the MSB

    const leb_Node staticNode = leb_Node(int(uint(vertexID / 3) | staticMask), staticDepth);
    return DecodeMeshletVertices(staticNode, vertexID % 3);
}

layout(location = 0) out vec2 o_TexCoord;
layout(location = 1) out vec4 o_WorldPos;

void main()
{
    const int lebID = u_LebID;

    vec2 i_VertexPos = GetMeshletVertex(gl_VertexID);

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

    const vec2 position = BarycentricInterpolation(trianglePositions, i_VertexPos);
    const vec4 worldPosition = planeToSphere(lebID, vec4(position, 1.0f, 1.0f));

    gl_Position = u_ProjMatrix * (u_ViewMatrix * worldPosition);
    o_TexCoord = position;
    o_WorldPos = worldPosition;
}
