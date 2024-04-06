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
uniform uvec2 size;
uniform vec4 axisLength;

layout(std430, binding = 0) readonly restrict buffer matrixBuffer {
    mat4 world[];
};

out mat4 vs_proj;
out uvec2 vs_size;
out vec4 vs_axisLength;

out vec4 vs_color;

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

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    const int instanceID = gl_InstanceID;

    // Take the last matrix in world[] if there are not enough in the buffer
    const int transformID = min(instanceID, world.length() - 1);

    const mat4 modelViewProj = proj * (view * world[transformID]);
    vec4[] positions = {
        modelViewProj * vec4(vertices[0], 1.0),
        modelViewProj * vec4(vertices[1], 1.0),
        modelViewProj * vec4(vertices[2], 1.0),
        modelViewProj * vec4(vertices[3], 1.0),
    };

    const float[] lengths = {
        0.0,
        length(size * (positions[1].xy - positions[0].xy)),
        length(size * (positions[2].xy - positions[0].xy)),
        length(size * (positions[3].xy - positions[0].xy)),
    };

    // Multiply by 0.5 because origin of object can be in the center of the screen
    const float maxLength = 0.5 * max(max(lengths[1], lengths[2]), lengths[3]);

    const float lengthFactor = axisLength.y / maxLength;
    const vec4 localPos = vec4(lengthFactor * vertices[indices[gl_VertexID]], 1.0);
    gl_Position = (view * (world[transformID] * localPos));

    vs_color = vec4(colors[gl_VertexID / 2], 1.0);

    vs_proj = proj;
    vs_size = size;
    vs_axisLength = axisLength;
}
