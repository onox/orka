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

uniform bool visible;

layout(location = 0) in vec4  vs_color;
layout(location = 1) in float vs_weight;

layout(location = 0) out vec4 fs_color;

// Experimentally determined numbers to get nice dashed lines
const float pattern_a = 1.0e3;
const int   pattern_b = 32;

void main() {
    float a = vs_weight * pattern_a;
    float b = float(int(a) % pattern_b);

    if (visible || b / pattern_b < 0.5) {
        fs_color = vs_color;
    }
    else {
        discard;
    }
}
