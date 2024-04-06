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

struct Line {
    vec4 from;
    vec4 to;
};

layout(std430, binding = 0) readonly restrict buffer matrixBuffer {
    mat4 world[];
};

layout(std430, binding = 1) readonly restrict buffer colorBuffer {
    vec4 colors[];
};

layout(std430, binding = 2) readonly restrict buffer lineBuffer {
    Line lines[];
};

layout(location = 0) out vec4  vs_color;
layout(location = 1) out float vs_weight;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    const int instanceID = gl_InstanceID;

    const vec3 vertices[] = {
        lines[instanceID].from.xyz,
        lines[instanceID].to.xyz
    };

    vec4 vertex = vec4(vertices[gl_VertexID], 1.0);

    // Take the last matrix in world[] if there are not enough in the buffer
    const int transformID = min(instanceID, world.length() - 1);
    const int colorID     = min(instanceID, colors.length() - 1);

    gl_Position = proj * (view * (world[transformID] * vertex));

    vs_weight = float(gl_VertexID % 2 != 0);
    vs_color  = colors[colorID];
}
