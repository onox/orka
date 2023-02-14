#version 420 core

uniform vec2 cursor;

void main(void) {
    gl_Position = vec4(cursor, 0.0, 1.0);
}
