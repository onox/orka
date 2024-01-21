#version 420 core

// SPDX-License-Identifier: MIT
//
// Copyright (c) 2023 onox <denkpadje@gmail.com>
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

layout(binding = 4) uniform sampler2D u_DmapSampler;

vec2 get_slope(vec2 texCoord) {
    const float queriedLod = textureQueryLod(u_DmapSampler, texCoord).y;
    const float lod = floor(queriedLod);

    const float l = textureLodOffset(u_DmapSampler, texCoord, lod, ivec2(-1,  0)).r;
    const float r = textureLodOffset(u_DmapSampler, texCoord, lod, ivec2( 1,  0)).r;
    const float b = textureLodOffset(u_DmapSampler, texCoord, lod, ivec2( 0, -1)).r;
    const float t = textureLodOffset(u_DmapSampler, texCoord, lod, ivec2( 0,  1)).r;

    const float x = 0.5 * (r - l);
    const float y = 0.5 * (t - b);

    return vec2(x, y);
}
