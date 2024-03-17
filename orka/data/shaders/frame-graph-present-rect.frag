#version 420 core

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

layout(binding = 0) uniform sampler2DRect colorTexture;

uniform vec4 screenResolution;
uniform bool applyGammaCorrection;

layout(location = 0) out vec4 fs_out;

const float gamma = 2.2;
const vec3 inverse_gamma = vec3(1.0 / gamma);

void main(void)
{
    const ivec2 P = ivec2(gl_FragCoord.xy * textureSize(colorTexture) / screenResolution.xy);

    vec4 color = texelFetch(colorTexture, P);

    if (color.a > 0.0) {
        color.rgb /= color.a;
    }

    if (applyGammaCorrection) {
        // Apply gamma correction due to monitor's non-linear pixel response
        fs_out = vec4(pow(color.rgb, inverse_gamma), 1.0);
    }
    else {
        fs_out = vec4(color.rgb, 1.0);
    }
}
