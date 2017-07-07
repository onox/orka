#version 330 core

#extension GL_ARB_shading_language_420pack : require

in VS_OUT {
    vec2 uv;
} fs_in;

out vec4 out_Color;

layout(binding = 0) uniform sampler2D colorTexture;

void main(void) {
    out_Color.rgb = vec3(texture(colorTexture, vec2(fs_in.uv)));
}
