//  SPDX-License-Identifier: Apache-2.0
//
//  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

uniform vec4 camera_offset = vec4(0.0);
uniform bool ground_hack = false;

uniform vec4 camera_pos;
uniform vec4 planet_pos;

uniform vec4 sun_direction;

uniform vec4 star_direction;
uniform float star_size; // cosine of angular radius

in vec3 view_ray;

layout(location = 0) out vec4 color;

#ifdef USE_LUMINANCE
#define GetSolarRadiance GetSolarLuminance
#define GetSkyRadiance GetSkyLuminance
#endif

vec3 GetSkyRadiance(vec3 camera, vec3 view_ray, float shadow_length,
    vec3 sun_direction, out vec3 transmittance, out bool intersects_ground);
vec3 GetSolarRadiance();

void main() {
    const vec3 view_direction = normalize(view_ray);

    // Hack 1: adjust position of camera to account for difference between
    // radius of semi-major and semi-minor axes
    const vec3 camera = camera_pos.xyz + camera_offset.yzx;

    // Radiance of the sky
    const float shadow_length = 0.0;
    const vec3 planet_to_camera = camera - planet_pos.xyz;
    vec3 transmittance;
    bool intersects_ground;
    vec3 radiance = GetSkyRadiance(planet_to_camera, view_direction,
        shadow_length, sun_direction.xyz, transmittance, intersects_ground);

    // Hack 2: undo the offset if the view ray intersects the ground, which
    // can happen because of the flattening of the sphere (if camera_offset /= 0)
    // (the atmosphere model assumes a non-flattened sphere)
    if (ground_hack && intersects_ground) {
        vec3 ground_transmittance;
        float offset_length = length(camera_offset.yzx);
        radiance = GetSkyRadiance(
            -offset_length * normalize(planet_to_camera) + planet_to_camera, view_direction,
            shadow_length, sun_direction.xyz, ground_transmittance, intersects_ground);
    }

    const float dotVS = max(0.0, dot(view_direction, star_direction.xyz));

    // Radiance of the Sun
    const float dotVSP = pow(dotVS, max(0.1, dotVS * 64));
    const float sunSize = star_size + 0.0000075 * smoothstep(0.0, 1.0, length(transmittance));

    const float ring_rad_factor = smoothstep(sunSize * dotVSP, sunSize, dotVSP);
    const float core_rad_factor = step(star_size, dotVS);
    radiance += max(ring_rad_factor, core_rad_factor) * transmittance * GetSolarRadiance();

    color = vec4(radiance, 1.0);
}
