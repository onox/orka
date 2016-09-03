#version 330 core

out vec4 out_color;

vec3 get_color(void);

void main(void) {
   out_color = vec4(get_color(), 1.0);
}
