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

out float weight;

//      6----7
//     /|   /|
//    / |  / |
//   /  4-/--5
//  /  / /  /
// 2----3  /
// | /  | /
// |/   |/
// 0----1
const vec3 vertices[] = {
    vec3(-1.0, -1.0, 1.0),
    vec3( 1.0, -1.0, 1.0),
    vec3(-1.0,  1.0, 1.0),
    vec3( 1.0,  1.0, 1.0),

    vec3(-1.0, -1.0, -1.0),
    vec3( 1.0, -1.0, -1.0),
    vec3(-1.0,  1.0, -1.0),
    vec3( 1.0,  1.0, -1.0),
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

void main() {
    const int instanceID = gl_BaseInstanceARB + gl_InstanceID;

    const vec4 bboxMin = bboxes[instanceID].minimum;
    const vec4 bboxMax = bboxes[instanceID].maximum;

    vec4 vertex = vec4(vertices[indices[gl_VertexID]], 1.0);
    vertex.xyz *= 0.5 * (bboxMax.xyz - bboxMin.xyz);

    gl_Position = proj * (view * (matrices[instanceID] * vertex));

    weight = float(gl_VertexID % 2 != 0);
}
