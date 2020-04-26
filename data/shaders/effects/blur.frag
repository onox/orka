#version 420 core

#extension GL_ARB_shader_storage_buffer_object : require

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
