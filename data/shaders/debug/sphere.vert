#version 420 core

#extension GL_ARB_shader_draw_parameters : require
#extension GL_ARB_shader_storage_buffer_object : require

// SPDX-License-Identifier: Apache-2.0
//
// Copyright (c) 2019 onox <denkpadje@gmail.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

uniform mat4 view;
uniform mat4 proj;

uniform int cellsHorizontal;
uniform int cellsVertical;

layout(std430, binding = 0) readonly restrict buffer matrixBuffer {
    mat4 world[];
};

layout(std430, binding = 1) readonly restrict buffer sizeBuffer {
    float sizes[];
};

out vec4 vs_normal;
out float vs_lon;
out float vs_lat;

const float pi = 3.14159265358979323846;

float lonStep = (2.0 * pi) / float(cellsHorizontal);
float latStep = (1.0 * pi) / float(cellsVertical);

vec3 get_vertex(float lonIndex, float latIndex) {
    float lonAngle = lonIndex * lonStep;
    float latAngle = pi / 2.0 - latIndex * latStep;

    float xy = cos(latAngle);
    float z = sin(latAngle);

    float x = xy * cos(lonAngle);
    float y = xy * sin(lonAngle);

    return normalize(vec3(x, y, z));
};

const uint lonOffset[] = {0, 0, 1, 0, 1, 1};
const uint latOffset[] = {0, 1, 0, 1, 0, 1};

void main() {
    const int instanceID = gl_BaseInstanceARB + gl_InstanceID;

    uint indexID     = gl_VertexID % 3;
    uint triangleID  = gl_VertexID / 3;
    uint quadIndexID = triangleID % 2 + indexID;

    // Add + 1 to the cellsHorizontal so that the first quad at row i+1
    // is below the last quad of row i
    float baseLon = (triangleID / 2) % (cellsHorizontal + 1);
    float baseLat = (triangleID / 2) / (cellsHorizontal + 1);

    float lonIndex = baseLon + lonOffset[quadIndexID];
    float latIndex = baseLat + latOffset[quadIndexID];

    vec4 vertex = vec4(get_vertex(lonIndex, latIndex), 1.0);
    vec3 normal = vertex.xyz;

    // Take the last size in sizes[] if there are not enough in the buffer
    const int sizeID = min(instanceID, sizes.length() - 1);

    vertex.xyz *= sizes[sizeID];

    gl_Position = proj * (view * (world[instanceID] * vertex));

    vs_normal = vec4(normal, 1.0);
    vs_lon = lonIndex;
    vs_lat = latIndex;
}
