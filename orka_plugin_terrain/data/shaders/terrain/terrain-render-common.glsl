#version 420 core

#extension GL_ARB_compute_shader : require

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

struct leb_Node {
    uint id;    // binary code
    int depth;  // subdivision depth
};

mat2x3 leb_DecodeNodeAttributeArray_Quad(in const leb_Node node, in const mat2x3 data);

// DecodeTriangleVertices -- Decodes the triangle vertices in local space
vec4[3] DecodeTriangleVertices(in const leb_Node node)
{
    vec3 xPos = vec3(0, 0, 1), yPos = vec3(1, 0, 0);
    mat2x3 pos = leb_DecodeNodeAttributeArray_Quad(node, mat2x3(xPos, yPos));
    vec4 p1 = vec4(pos[0][0], pos[1][0], 0.0, 1.0);
    vec4 p2 = vec4(pos[0][1], pos[1][1], 0.0, 1.0);
    vec4 p3 = vec4(pos[0][2], pos[1][2], 0.0, 1.0);

    return vec4[3](p1, p2, p3);
}

// Compute a barycentric interpolation
vec2 BarycentricInterpolation(in vec2 v[3], in vec2 u)
{
    return v[1] + u.x * (v[2] - v[1]) + u.y * (v[0] - v[1]);
}

vec4 BarycentricInterpolation(in vec4 v[3], in vec2 u)
{
    return v[1] + u.x * (v[2] - v[1]) + u.y * (v[0] - v[1]);
}

vec2 to_origin_upper_left(in const vec2 value) {
    return vec2(value.x, 1.0 - value.y);
}

const float RPI = 1.0 / 3.141592653589793;

// Return the UV mapping of the longitude and latitude of the given point
// (relative to the center of the planet)
//
// U = 0.0 .. 1.0 for longitude between -180.0 deg .. +180.0 deg
// V = 0.0 .. 1.0 for latitude  between  -90.0 deg ..  +90.0 deg
vec2 get_normalized_lonlat(in const vec3 point) {
    const float dotY = dot(point, vec3(0, 1, 0));
    const float coordLat = 1.0 - acos(dotY) * RPI;

    const vec3 pointEquator = normalize(vec3(point.x, 0.0, point.z));

    const float dotZ = dot(pointEquator, vec3(0, 0, 1));
    const float dotX = dot(pointEquator, vec3(1, 0, 0));

    const float halfAngle = acos(dotZ) * RPI;
    const float coordLon = ((dotX > 0.0 ? halfAngle : -halfAngle) + 1.0) * 0.5;

    return vec2(coordLon, coordLat);
}
