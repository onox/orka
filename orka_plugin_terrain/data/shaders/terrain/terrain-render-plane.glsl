#version 420 core

#extension GL_ARB_compute_shader               : require
#extension GL_ARB_shader_storage_buffer_object : require

// SPDX-License-Identifier: MIT
//
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

vec2 to_origin_upper_left(in const vec2 value);

layout(std430, binding = 3) readonly restrict buffer ModelMatrixBuffer {
    mat4 modelMatrix[];
};

layout(binding = 4) uniform sampler2D u_DmapSampler;

layout(std140, binding = 1) uniform MetadataBuffer {
    vec2 u_DmapFactor;
    float u_MinLodVariance;
    float u_LodFactor;
};

vec4 get_local_direction(vec2 vertex) {
    vec3 v = vec3(vertex, 1.0);

    // v = (H, V, 1.0, 1.0) where H and V span a plane
    v.xy = v.xy * 2.0 - 1.0;

    return vec4(v, 1.0);
}

vec4 get_world_pos(const in int lebID, const in vec4 localDirection, const in float heightAboveSurface) {
    const vec3 scaleFactor = vec3(1.0);
    return modelMatrix[lebID] * (vec4(scaleFactor + heightAboveSurface, 1.0) * localDirection);
}

vec4 planeToSphere(const int lebID, vec2 vertex, out vec2 lonLatUV) {
    const vec4 localDirection = get_local_direction(vertex);

    const vec2 uv = vertex;
    const float height = u_DmapFactor.x * texture(u_DmapSampler, to_origin_upper_left(uv)).r - u_DmapFactor.y;

    lonLatUV = uv;
    return get_world_pos(lebID, localDirection, height);
}
