#version 420 core

#extension GL_EXT_shader_samples_identical : require
#extension GL_ARB_shader_texture_image_samples : require

// SPDX-License-Identifier: Apache-2.0
//
// Copyright (c) 2021 onox <denkpadje@gmail.com>
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

layout(origin_upper_left) in vec4 gl_FragCoord;

layout(binding = 0) uniform sampler2DMS colorTexture;

uniform vec4 screenResolution;
uniform float exposure;

layout(location = 0) out vec4 fs_out;

const float gamma = 2.2;
const vec3 inverse_gamma = vec3(1.0 / gamma);

void main(void)
{
    const ivec2 P = ivec2(gl_FragCoord.xy * textureSize(colorTexture) / screenResolution.xy);

    const int samples = textureSamples(colorTexture);

    // Resolve MSAA samples
    vec4 color = texelFetch(colorTexture, P, 0);
    // Should reduce bandwidth (enabled on iris and radeonsi)
    if (!textureSamplesIdenticalEXT(colorTexture, P)) {
        for (int i = 1; i < samples; ++i) {
            vec4 c = texelFetch(colorTexture, P, i);
            color += vec4(c.a * c.rgb, c.a);
        }
    }

    if (color.a > 0.0) {
        color.rgb /= color.a;
    }

    // Reinhard tonemapping
    color.rgb *= exposure;
    color.rgb /= 1.0 + color.rgb;

    // Apply gamma correction due to monitor's non-linear pixel response
    fs_out = vec4(pow(color.rgb, inverse_gamma), 1.0);
}
