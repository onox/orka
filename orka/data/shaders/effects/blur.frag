#version 420 core

#extension GL_ARB_shader_storage_buffer_object : require

layout(origin_upper_left) in vec4 gl_FragCoord;

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

layout(origin_upper_left) in vec4 gl_FragCoord;

uniform bool horizontal;

layout(binding = 0) uniform sampler2DRect inputImage;

// A buffer consisting of a number of offsets, followed by a number
// of weights
layout(std430, binding = 0) restrict buffer KernelBuffer {
    float offsetsAndWeights[];
};
int weights = offsetsAndWeights.length() / 2;

out vec4 outputImage;

void main(void) {
    const vec2 pixel = gl_FragCoord.xy;

    const int h = horizontal ? 1 : 0;
    const int v = horizontal ? 0 : 1;

    const vec4 inputPixel = texture(inputImage, pixel);

    // Center weight
    vec4 sum = inputPixel * offsetsAndWeights[weights + 0];

    // Non-center weights of the bell curve
    for (int i = 1; i < weights; i++) {
        const float o = offsetsAndWeights[i];
        const float kernelValue = offsetsAndWeights[weights + i];
        sum += kernelValue * texture(inputImage, vec2(pixel.x + o * h, pixel.y + o * v));
        sum += kernelValue * texture(inputImage, vec2(pixel.x - o * h, pixel.y - o * v));
    }

    outputImage = sum;
}
