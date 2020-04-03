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
uniform vec4 camera_pos;
uniform vec4 planet_pos;

uniform vec4 sun_direction;
uniform float sun_size; // cosine of angular radius

uniform float exposure;

in vec3 view_ray;

layout(location = 0) out vec4 color;

#ifdef USE_LUMINANCE
#define GetSolarRadiance GetSolarLuminance
#define GetSkyRadiance GetSkyLuminance
#endif

vec3 GetSkyRadiance(vec3 camera, vec3 view_ray, float shadow_length,
    vec3 sun_direction, out vec3 transmittance);
vec3 GetSolarRadiance();

void main() {
    const vec3 view_direction = normalize(view_ray);
    const vec3 camera = camera_pos.xyz + camera_offset.yzx;

    // Radiance of the sky
    const float shadow_length = 0.0;
    vec3 transmittance;
    vec3 radiance = GetSkyRadiance(camera - planet_pos.xyz, view_direction,
        shadow_length, sun_direction.xyz, transmittance);

    const float dotVS = max(0.0, dot(view_direction, sun_direction.xyz));

    // Radiance of the Sun
    const float dotVSP = pow(dotVS, max(1.0, dotVS * 64 * length(transmittance)));
    const float sunSize = sun_size + 0.000005 * smoothstep(0.0, 0.5, length(transmittance));
    const float solar_rad_factor = smoothstep(sunSize * dotVSP, sunSize, dotVSP);

    radiance += solar_rad_factor * transmittance * GetSolarRadiance();

    color = vec4(vec3(1.0) - exp(-radiance * exposure), 1.0);
}
