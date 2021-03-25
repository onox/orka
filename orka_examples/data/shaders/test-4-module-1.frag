// SPDX-License-Identifier: Apache-2.0
#version 420 core

flat in uint ex_InstanceID;

out vec4 out_Color;

const vec4[] colors = {
    vec4(1.0, 0.0, 0.0, 1.0), // red
    vec4(0.0, 1.0, 0.0, 1.0), // green
    vec4(0.0, 0.0, 1.0, 1.0), // blue
    vec4(1.0, 1.0, 0.0, 1.0), // yellow
    vec4(1.0, 0.0, 1.0, 1.0), // magenta
    vec4(0.0, 1.0, 1.0, 1.0), // cyan
    vec4(1.0, 1.0, 1.0, 1.0), // white
};
const uint max_color_index = uint(colors.length() - 1);

void main(void) {
    const uint draw = clamp(ex_InstanceID, 0u, max_color_index);
    out_Color = colors[draw];
}
