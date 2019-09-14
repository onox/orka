#version 420 core

#extension GL_ARB_enhanced_layouts : require

layout(points) in;
layout(triangle_strip, max_vertices = 3) out;

in float geom_value[];
layout(xfb_buffer = 0, xfb_offset = 0) out float out_value;

void main()
{
    for (int i = 0; i < 3; i++)
    {
        out_value = geom_value[0] + i;
        EmitVertex();
    }

    EndPrimitive();
}
