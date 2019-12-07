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

layout(std430, binding = 0) readonly restrict buffer matrixBuffer {
    mat4 world[];
};

layout(std430, binding = 1) readonly restrict buffer sizeBuffer {
    float sizes[];
};

out vec4  vs_color;
out float vs_weight;

const vec3 vertices[] = {
    vec3(0.0, 0.0, 0.0),
    vec3(1.0, 0.0, 0.0),
    vec3(0.0, 1.0, 0.0),
    vec3(0.0, 0.0, 1.0),
};

const int indices[] = {
    0, 1, // x-axis
    0, 2, // y-axis
    0, 3  // z-axis
};

const vec3 colors[] = {
    vec3(1.0, 0.0, 0.0),
    vec3(0.0, 1.0, 0.0),
    vec3(0.0, 0.0, 1.0),
};

void main() {
    const int instanceID = gl_BaseInstanceARB + gl_InstanceID;

    // Take the last matrix in world[] if there are not enough in the buffer
    const int transformID = min(instanceID, world.length() - 1);
    const int sizeID      = min(instanceID, sizes.length() - 1);

    vec4 vertex = vec4(vertices[indices[gl_VertexID]], 1.0);
    vertex.xyz *= sizes[sizeID];

    gl_Position = proj * (view * (world[transformID] * vertex));

    vs_color  = vec4(colors[gl_VertexID / 2], 1.0);
    vs_weight = float(gl_VertexID % 2 != 0);
}
