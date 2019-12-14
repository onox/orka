#version 420 core

// SPDX-License-Identifier: Apache-2.0
//
// Copyright (c) 2019 onox <denkpadje@gmail.com>
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

in vec3 vs_uvw;
in vec3 vs_color;
in vec3 vs_world;

layout(location = 0) out vec4 fs_color;

uniform mat4 view;
uniform mat4 proj;

layout(binding = 0) uniform sampler3D colorTexture;

const float volume_alpha = 0.5;

void main(void) {
    const vec3 view_dir = normalize(vs_world);

    fs_color = vec4(vec3(0.0), 0.0);

    for (int i = 100; i >= 0; i--) {
        vec4 src_color = texture(colorTexture, vs_uvw + view_dir * float(i) * 0.01);
        src_color.a = volume_alpha;

        // src_alpha, one_minus_src_alpha
        fs_color = vec4(src_color.a) * src_color + vec4(1.0 - src_color.a) * fs_color;
    }

    // Add fixed color to ease identification of the faces of the cube
    fs_color.xyz += vec3(vs_color) * 0.1;
    fs_color.a = 1.0;
}
