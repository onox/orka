#version 420 core

//  SPDX-License-Identifier: Apache-2.0
//
//  Copyright (c) 2020 onox <denkpadje@gmail.com>
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.

uniform mat4 view;
uniform mat4 proj;

out vec3 view_ray;

// Use 1 oversized triangle instead of 2 triangles forming a quad
// This prevents overshading along the diagonal and better cache
// efficiency on certain hardware architectures [1]
//
// References:
//
// [1] https://michaldrobot.com/2014/04/01/gcn-execution-patterns-in-full-screen-passes/
const vec2 vertices[] = {
    vec2(-1.0, -1.0),
    vec2( 3.0, -1.0),
    vec2(-1.0,  3.0),
};

void main() {
    vec4 vertex = vec4(vertices[gl_VertexID], 0.0, 1.0);

    view_ray = (inverse(view) * (inverse(proj) * vertex)).xyz;
    gl_Position = vertex;
}
