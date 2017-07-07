#version 330 core

#extension GL_ARB_shading_language_420pack : require

in VS_OUT {
    vec2 uv;
} fs_in;

out vec4 out_color;

uniform vec4 screenSize;
uniform vec4 imageSize;

layout(binding = 0) uniform sampler2D colorTexture;

void main(void) {
    const vec2 ratio = screenSize.xy / imageSize.xy;
    const vec2 image_size = imageSize.xy * min(ratio.x, ratio.y);

    const vec2 offset = (screenSize.xy - image_size) / 2.0;

    const vec2 begin_uv = offset / screenSize.xy;
    const vec2 end_uv = vec2(1.0) - begin_uv;

    const bool e1 = all(greaterThanEqual(fs_in.uv, begin_uv));
    const bool e2 = all(lessThanEqual(fs_in.uv, end_uv));

    const vec2 image_uv = clamp((fs_in.uv - begin_uv) / (end_uv - begin_uv), 0.0, 1.0);
    out_color.rgb = float(e1 && e2) * vec3(texture(colorTexture, image_uv));
}
