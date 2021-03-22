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

layout(binding = 0) uniform sampler2DArray colorTexture;

// Give some slight preference to wide windows
const float horizontal_bias = 1.5;

void main(void) {
    const vec3 image_size = textureSize(colorTexture, 0);
    const vec2 ratio = screenSize.xy / image_size.xy;

    float images_x_a = clamp(round(ratio.x), 1.0, image_size.z);
    const float images_y_a  = ceil(image_size.z / images_x_a);
    images_x_a = ceil(image_size.z / images_y_a);

    float images_y_b = clamp(round(ratio.y), 1.0, image_size.z);
    const float images_x_b  = ceil(image_size.z / images_y_b);
    images_y_b = ceil(image_size.z / images_x_b);

    vec2 count;
    if (images_x_a - horizontal_bias <= images_x_b) {
        count = vec2(images_x_a, images_y_a);
    }
    else {
        count = vec2(images_x_b, images_y_b);
    }

    const vec2 total_image_size = image_size.xy * count;
    const vec2 total_ratio = screenSize.xy / total_image_size;
    const float min_total_ratio = min(total_ratio.x, total_ratio.y);

    const float scale_ratio = useBestFit ? min_total_ratio : min(1.0, min_total_ratio);
    const vec2 scaled_image_size = total_image_size * scale_ratio;

    // Compute the size of the black bars left/right or above/below the image
    const vec2 offset = (screenSize.xy - scaled_image_size) / 2.0;

    // Compute the normalized clip coordinates to position the image in the center
    const vec2 begin_uv = offset / screenSize.xy;
    const vec2 end_uv = vec2(1.0) - begin_uv;

    const vec2 image_uv = clamp((fs_in.uv - begin_uv) / (end_uv - begin_uv), 0.0, 1.0);

    const vec2 modulus = vec2(1.0) / count;
    const vec2 layerUV = floor(image_uv / modulus);
    const float layer = layerUV.y * count.x + layerUV.x;

    const bool e1 = all(greaterThanEqual(fs_in.uv, begin_uv));
    const bool e2 = all(lessThanEqual(fs_in.uv, end_uv));
    const bool e3 = layer < image_size.z;

    const vec3 uvw = vec3(mod(image_uv, modulus) * count, layer);
    fs_color.rgb = float(e1 && e2 && e3) * vec3(texture(colorTexture, uvw));
    fs_color.a   = 1.0;
}
