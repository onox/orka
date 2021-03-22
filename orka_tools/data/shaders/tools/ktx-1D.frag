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

in VS_OUT {
    vec2 uv;
} fs_in;

layout(location = 0) out vec4 fs_color;

uniform vec4 screenSize;

uniform bool useBestFit;

layout(binding = 0) uniform sampler1D colorTexture;

void main(void) {
    const float image_width = textureSize(colorTexture, 0);

    const float ratio = screenSize.x / image_width;
    const float scale_ratio = useBestFit ? ratio : min(1.0, ratio);
    const vec2 scaled_image_size = vec2(image_width, 1.0) * scale_ratio;

    // Compute the size of the black bars left/right or above/below the image
    const vec2 offset = (screenSize.xy - scaled_image_size) / 2.0;

    // Compute the normalized clip coordinates to position the image in the center
    const vec2 begin_uv = offset / screenSize.xy;
    const vec2 end_uv = vec2(1.0) - begin_uv;

    const bool e1 = all(greaterThanEqual(fs_in.uv, begin_uv));
    const bool e2 = all(lessThanEqual(fs_in.uv, end_uv));

    const vec2 image_uv = clamp((fs_in.uv - begin_uv) / (end_uv - begin_uv), 0.0, 1.0);
    fs_color.rgb = float(e1 && e2) * vec3(texture(colorTexture, image_uv.x));
    fs_color.a = 1.0;
}
