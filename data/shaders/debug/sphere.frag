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

uniform vec4 color;

uniform bool useNormal;
uniform bool visible;

in vec4 vs_normal;
in float vs_lon;
in float vs_lat;

layout(location = 0) out vec4 fs_color;

float opacityFactor = visible ? 1.0 : 0.1;

void main() {
    // Discard diagonal lines to get quads
    if (!(round(vs_lon) == vs_lon || round(vs_lat) == vs_lat)) {
        discard;
    }

    fs_color = useNormal ? vs_normal : color;
    fs_color.a = max(fs_color.a, 0.1) * opacityFactor;
}
