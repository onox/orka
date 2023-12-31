#version 420 core

// SPDX-License-Identifier: MIT
//
// Copyright (c) 2019 Jonathan Dupuy
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

#define FLAG_DISPLACE 1

vec2 get_slope(vec2 texCoord);
vec2 to_origin_upper_left(in const vec2 value);

uniform float u_DmapFactor;

vec4 ShadeFragment(vec2 texCoord, vec4 worldPos)
{
#if FLAG_DISPLACE
    vec2 smap = u_DmapFactor * get_slope(to_origin_upper_left(texCoord));
    vec3 n = normalize(vec3(-smap, 1));
#else
    vec3 n = vec3(0, 0, 1);
#endif

    float d = clamp(n.z, 0.0, 1.0);
    return vec4(vec3(d / 3.14159), 1);
}
