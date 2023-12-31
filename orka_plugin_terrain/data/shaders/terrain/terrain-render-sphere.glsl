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
vec2 get_normalized_lonlat(in const vec3 point);

struct Spheroid {
    float axis;
    float eccentricitySquared;
    vec2 xyMask;
};

layout(std430, binding = 3) readonly restrict buffer ModelMatrixBuffer {
    mat4 modelMatrix[];
};

layout(std430, binding = 4) readonly restrict buffer SphereBuffer {
    Spheroid spheres[];
};

layout(binding = 4) uniform sampler2D u_DmapSampler;
uniform float u_DmapFactor;

// Return the scale factor to convert geodetic coordinates to geocentric (ECEF)
//
// See https://en.wikipedia.org/wiki/Geographic_coordinate_conversion#From_geodetic_to_ECEF_coordinates
vec3 geodetic_to_geocentric_scale_factor(in const Spheroid sphere, in const float heightAboveEquator)
{
    const float e2 = sphere.eccentricitySquared;

    // Radius of curvature in the prime vertical
    // (distance between surface and z-axis along the normal at the surface)
    const float N = sphere.axis * inversesqrt(1.0 - e2 * heightAboveEquator * heightAboveEquator);

    // 1 - e2 = b**2 / a**2
    return (1.0 - e2 * vec3(sphere.xyMask, 0)) * N;
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
vec2 warp_tan(vec2 x) {
    return tan(warp_theta * x) * rtan_warp_theta;
}

// COBE quadrilateralized bivariate warp (Equation 16)
vec2 warp_cobe(vec2 x) {
   const vec2 y  = x.yx;
   const vec2 x2 = x * x;
   const vec2 y2 = y * y;
   const vec2 sum = ((-0.0941180085824 + 0.0409125981187 * y2 - 0.0623272690881 * x2) * x2 +
                      (0.0275922480902 + 0.0342217026979 * y2) * y2);
   return (0.723951234952 + 0.276048765048 * x2 + (1.0 - x2) * sum) * x;
}

// Everitt's univariate invertible warp with e = 1.4511 (Equation 14)
vec2 warp_everitt(vec2 x) {
    return sign(x) * ((1.4511 - sqrt(2.10569121 - 1.8044 * abs(x))) / 0.9022);
}

vec4 get_local_direction(vec2 vertex) {
    vec3 v = vec3(vertex, 1.0);

    // v = (H, V, 1.0, 1.0) where H and V span a plane
    v.xy = v.xy * 2.0 - 1.0;
    v.xy = warp_everitt(v.xy);

    return vec4(normalize(v), 1.0);
}

vec4 get_world_pos(const in int lebID, vec4 localDirection, const in float heightAboveSurface) {
    // Take the last size in spheres[] if there are not enough in the buffer
    const int sizeID = min(lebID, spheres.length() - 1);
    const Spheroid sphere = spheres[sizeID];

    const float heightAboveEquator = length(localDirection.xy * sphere.xyMask);
    const vec3 scaleFactor = geodetic_to_geocentric_scale_factor(sphere, heightAboveEquator);
    return modelMatrix[lebID] * (vec4(scaleFactor + heightAboveSurface, 1.0) * localDirection);
}

vec4 planeToSphere(const int lebID, vec2 vertex) {
    const vec4 localDirection = get_local_direction(vertex);
    const vec4 worldPos = modelMatrix[lebID] * localDirection;

    const vec2 uv = get_normalized_lonlat(worldPos.xyz);
    const float height = u_DmapFactor * texture(u_DmapSampler, to_origin_upper_left(uv)).r;
    return get_world_pos(lebID, localDirection, height);
}
