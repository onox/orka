#version 330 core

#extension GL_ARB_shading_language_420pack : require

struct Vertex {
    vec2 position;
    vec2 uv;
};

// Use 1 oversized triangle instead of 2 triangles forming a quad
// This prevents overshading along the diagonal and better cache
// efficiency on certain hardware architectures
const Vertex vertices[] = {
    {vec2(-1.0, -1.0), vec2(0.0, 0.0)},
    {vec2( 3.0, -1.0), vec2(2.0, 0.0)},
    {vec2(-1.0,  3.0), vec2(0.0, 2.0)},
};

const int indices[] = {
    0, 1, 2
};

out VS_OUT {
    vec2 uv;
} vs_out;

void main(void) {
    const Vertex vertex = vertices[indices[gl_VertexID]];
    gl_Position = vec4(vertex.position, 0.0, 1.0);
    vs_out.uv = vertex.uv;
}
