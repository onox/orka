#version 420 core

in VS_OUT {
    vec2 uv;
} fs_in;

out vec4 outColor;

layout(binding = 0) uniform sampler2D colorTexture;
uniform int effect;

const vec3 color_grey  = vec3(0.2126, 0.7152, 0.0722);
const vec3 color_sepia = vec3(1.2, 1.0, 0.8);

const float vignette_radius = 0.5;
const float vignette_softness = 0.45;

void main()
{
    outColor = texture(colorTexture, fs_in.uv);

    if (effect == 1) {
        float avg = dot(outColor, vec4(color_grey, 0.0));
        outColor = vec4(avg, avg, avg, 1.0);
    }
    else if (effect == 2) {
        // Add vignette effect
        float len = length(fs_in.uv.xy - vec2(0.5));
        float vignette = smoothstep(vignette_radius, vignette_radius - vignette_softness, len);
        outColor = mix(outColor, outColor * vignette, 0.7);

        // Make image grey and then give it sepia colors
        float grey_avg = dot(outColor, vec4(color_grey, 0.0));
        vec3 sepia = vec3(grey_avg) * color_sepia;
        outColor = vec4(sepia, 1.0);
    }
}
