#version 420 core

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

uniform vec4 color;

struct BoundingBox {
    vec4 minimum;
    vec4 maximum;
};

layout(std430, binding = 0) readonly restrict buffer matrixBuffer {
    mat4 matrices[];
};

layout(std430, binding = 1) readonly restrict buffer bboxBuffer {
    BoundingBox bboxes[];
};

layout(location = 0) out vec4  vs_color;
layout(location = 1) out float vs_weight;

// For each vertex, choose bboxMax if true and bboxMin if false
//
//      6----7
//     /|   /|
//    / |  / |
//   /  4-/--5
//  /  / /  /
// 2----3  /
// | /  | /
// |/   |/
// 0----1
const bvec3 vertices[] = {
    bvec3(false, false, true),
    bvec3( true, false, true),
    bvec3(false,  true, true),
    bvec3( true,  true, true),

    bvec3(false, false, false),
    bvec3( true, false, false),
    bvec3(false,  true, false),
    bvec3( true,  true, false),
};

const int indices[] = {
    // Front
    0, 1,
    0, 2,
    1, 3,
    2, 3,

    // Back
    4, 5,
    4, 6,
    5, 7,
    6, 7,

    // Right
    1, 5,
    3, 7,

    // Left
    0, 4,
    2, 6
};

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    const int instanceID = gl_InstanceID;

    const vec3 bboxMin = bboxes[instanceID].minimum.xyz;
    const vec3 bboxMax = bboxes[instanceID].maximum.xyz;

    const vec3 vertex = mix(bboxMin, bboxMax, vertices[indices[gl_VertexID]]);

    gl_Position = proj * (view * (matrices[instanceID] * vec4(vertex, 1.0)));

    vs_color  = color;
    vs_weight = float(gl_VertexID % 2 != 0);
}
