// SPDX-License-Identifier: Apache-2.0
#version 420 core

struct Vertex {
    vec2 position;
    vec2 texcoord;
};

const Vertex in_vertices[] = {
    Vertex(vec2(-1.0,  1.0), vec2(0.0, 1.0)),
    Vertex(vec2( 1.0,  1.0), vec2(1.0, 1.0)),
    Vertex(vec2( 1.0, -1.0), vec2(1.0, 0.0)),
    Vertex(vec2(-1.0, -1.0), vec2(0.0, 0.0)),
};

out vec2 Texcoord;

void main()
{
    const Vertex data = in_vertices[gl_VertexID];

    Texcoord = data.texcoord;
    gl_Position = vec4(data.position, 0.0, 1.0);
}
