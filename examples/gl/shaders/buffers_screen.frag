// SPDX-License-Identifier: Apache-2.0
#version 420 core

in vec2 Texcoord;

out vec4 outColor;

layout(binding = 0) uniform sampler2D texFrameBuffer;
uniform int effect;

const float blurSizeH = 1.0 / 300.0;
const float blurSizeV = 1.0 / 200.0;

const vec3 color_grey  = vec3(0.2126, 0.7152, 0.0722);
const vec3 color_sepia = vec3(1.2, 1.0, 0.8);

const float vignette_radius = 0.5;
const float vignette_softness = 0.45;

void main()
{
    outColor = texture(texFrameBuffer, Texcoord);

    if (effect == 1) {
        float avg = dot(outColor, vec4(color_grey, 0.0));
        outColor = vec4(avg, avg, avg, 1.0);
    }
    else if (effect == 2) {
        // Add vignette effect
        float len = length(Texcoord.xy - vec2(0.5));
        float vignette = smoothstep(vignette_radius, vignette_radius - vignette_softness, len);
        outColor = mix(outColor, outColor * vignette, 0.7);

        // Make image grey and then give it sepia colors
        float grey_avg = dot(outColor, vec4(color_grey, 0.0));
        vec3 sepia = vec3(grey_avg) * color_sepia;
        outColor = vec4(sepia, 1.0);
    }
    else if (effect == 3) {
        vec4 sum = vec4(0.0);
        for (int x = -4; x <= 4; x++)
        {
            for (int y = -4; y <= 4; y++)
            {
                outColor = texture(texFrameBuffer, vec2(Texcoord.x + x * blurSizeH, Texcoord.y + y * blurSizeV));

                // Make screen gray
                float avg = dot(outColor, vec4(color_grey, 0.0));
                outColor = vec4(avg, avg, avg, 1.0);

                sum += outColor / 81.0;
            }
        }
        outColor = sum;
    }
    else if (effect == 4) {
        // Sobel
        vec4 s1 = texture(texFrameBuffer, Texcoord - 1.0 / 300.0 - 1.0 / 200.0);
        vec4 s2 = texture(texFrameBuffer, Texcoord + 1.0 / 300.0 - 1.0 / 200.0);
        vec4 s3 = texture(texFrameBuffer, Texcoord - 1.0 / 300.0 + 1.0 / 200.0);
        vec4 s4 = texture(texFrameBuffer, Texcoord + 1.0 / 300.0 + 1.0 / 200.0);
        vec4 sx = 4.0 * ((s4 + s3) - (s2 + s1));
        vec4 sy = 4.0 * ((s2 + s4) - (s1 + s3));
        vec4 sobel = sqrt(sx * sx + sy * sy);
        outColor = sobel;
        outColor.w = 1.0;
    }
}
