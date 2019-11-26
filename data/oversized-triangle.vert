#version 420 core

#extension GL_ARB_shading_language_420pack : require

//  SPDX-License-Identifier: Apache-2.0
//
//  Copyright (c) 2018 onox <denkpadje@gmail.com>
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

struct Vertex {
    vec2 position;
    vec2 uv;
};

// Use 1 oversized triangle instead of 2 triangles forming a quad
// This prevents overshading along the diagonal and better cache
// efficiency on certain hardware architectures [1]
//
// References:
//
// [1] https://michaldrobot.com/2014/04/01/gcn-execution-patterns-in-full-screen-passes/
const Vertex vertices[] = {
    {vec2(-1.0, -1.0), vec2(0.0, 0.0)},
    {vec2( 3.0, -1.0), vec2(2.0, 0.0)},
    {vec2(-1.0,  3.0), vec2(0.0, 2.0)},
};

out VS_OUT {
    vec2 uv;
} vs_out;

void main(void) {
    const Vertex vertex = vertices[gl_VertexID];
    gl_Position = vec4(vertex.position, 0.0, 1.0);
    vs_out.uv = vertex.uv;
}
