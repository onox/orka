#version 420 core

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

uniform bool showExternal;
uniform bool showColors;

out vec3 vs_uvw;
out vec3 vs_color;

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
    0, 1, 2,
    2, 1, 3,

    // Back
    5, 4, 7,
    7, 4, 6,

    // Right
    1, 5, 3,
    3, 5, 7,

    // Left
    4, 0, 6,
    6, 0, 2,

    // Top
    2, 3, 6,
    6, 3, 7,

    // Bottom
    0, 4, 1,
    1, 4, 5,
};

const vec3 colors[] = {
    // Front
    vec3(0.0, 0.0, 1.0), // blue

    // Back
    vec3(0.0, 1.0, 1.0), // cyan

    // Right
    vec3(1.0, 0.0, 0.0), // red

    // Left
    vec3(1.0, 0.0, 1.0), // magenta

    // Top
    vec3(0.0, 1.0, 0.0), // green

    // Bottom
    vec3(1.0, 1.0, 0.0), // yellow
};

void main() {
    mat4 translate = mat4(1.0);
    translate[3].z = showExternal ? -4.0 : 0.0;

    const vec3 vertex = vertices[indices[gl_VertexID]];
    gl_Position = proj * (translate * view * vec4(vertex, 1.0));

    vs_color = showColors ? colors[gl_VertexID / 6] : vec3(0.0);
    vs_uvw = vertex;
}
