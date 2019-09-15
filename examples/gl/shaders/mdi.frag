// SPDX-License-Identifier: Apache-2.0
#version 330 core

#extension GL_ARB_shading_language_420pack : require

flat in uint ex_InstanceID;
out vec4 out_Color;

const vec4[] colors = {
    vec4(1.0, 0.0, 0.0, 1.0),
    vec4(0.0, 1.0, 0.0, 1.0),
    vec4(0.0, 0.0, 1.0, 1.0),
    vec4(1.0, 1.0, 0.0, 1.0),
    vec4(1.0, 0.0, 1.0, 1.0),
    vec4(0.0, 1.0, 1.0, 1.0),
    vec4(1.0, 1.0, 1.0, 1.0)
};
const uint max_color_index = uint(colors.length() - 1);

void main(void) {
    const uint draw = clamp(ex_InstanceID, 0u, max_color_index);
    out_Color = colors[draw];
}
