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

struct Spheroid {
    float axis;
    float eccentricitySquared;
    vec2 yzMask;
};

layout(std430, binding = 3) readonly restrict buffer ModelMatrixBuffer {
    mat4 modelMatrix[];
};

layout(std430, binding = 4) readonly restrict buffer SphereBuffer {
    Spheroid spheres[];
};

layout(binding = 4) uniform sampler2D u_DmapSampler;
uniform float u_DmapFactor;

vec3 get_sphere_scale(in const Spheroid sphere, in const float height)
{
    const float e2 = sphere.eccentricitySquared;

    // Radius of curvature in the prime vertical
    const float N = sphere.axis / sqrt(1.0 - e2 * height * height);

    // 1 - e2 = b**2 / a**2
    return (1.0 - e2 * vec3(0, sphere.yzMask)) * N;
}

// Various warp functions [1] to reduce RMSE when projecting cubes on spheres.
//
// [1] M. Zucker and Y. Higashi, Cube-to-sphere Projections for Procedural Texturing and Beyond,
//     Journal of Computer Graphics Techniques (JCGT), vol. 7, no. 2, 1–22, 2018
//     http://jcgt.org/published/0007/02/01/

// Tangent warp function
const float warp_theta = 0.618734829276; // (makes quads near the edge more squarish)
// const float warp_theta = 0.868734829276; // (optimal, lowest RMSE)
const float rtan_warp_theta = 1.0 / tan(warp_theta);

// Note: tan() function can give gaps between the borders of adjacent terrain tiles
vec2 warp_tan(vec2 x)
{
    return tan(warp_theta * x) * rtan_warp_theta;
}

// Everitt's univariate invertible warp with e = 1.4511 (Equation 14)
vec2 warp_everitt(vec2 x)
{
   return sign(x) * ((1.4511 - sqrt(2.10569121 - 1.8044 * abs(x))) / 0.9022);
}

vec4 planeToSphere(const int lebID, vec4 v)
{
    const float z = u_DmapFactor * texture(u_DmapSampler, v.xy).r;

    // v = (H, V, 1.0, 1.0) where H and V span a plane
    v.xy = v.xy * 2.0 - 1.0;
    v.xy = warp_everitt(v.xy);

    const vec4 unit = vec4(normalize(v.xyz), 1.0);

    // Take the last size in spheres[] if there are not enough in the buffer
    const int sizeID = min(lebID, spheres.length() - 1);
    const Spheroid sphere = spheres[sizeID];

    const float height = length(unit.yz * sphere.yzMask);
    return modelMatrix[lebID] * (vec4(get_sphere_scale(sphere, height) + vec3(z), 1.0) * unit);
}
