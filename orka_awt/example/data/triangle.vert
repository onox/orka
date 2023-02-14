#version 420 core

struct Vertex {
    vec4 position;
    vec4 color;
};

const Vertex vertices[] = {
    Vertex(vec4(-0.5, -0.5, 0.0, 1.0), vec4(1.0, 0.0, 0.0, 0.0)),
    Vertex(vec4( 0.5, -0.5, 0.0, 1.0), vec4(0.0, 1.0, 0.0, 0.0)),
    Vertex(vec4( 0.0,  0.5, 0.0, 1.0), vec4(0.0, 0.0, 1.0, 0.0))
};

out vec3 vs_color;

void main(void) {
    gl_Position = vertices[gl_VertexID].position;
    vs_color = vertices[gl_VertexID].color.xyz;
}
