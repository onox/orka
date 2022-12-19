#version 420 core

// SPDX-License-Identifier: MIT
//
// Copyright (c) 2019 Jonathan Dupuy
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of
// this software and associated documentation files (the "Software"), to deal in
// the Software without restriction, including without limitation the rights to
// use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do
// so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

vec4 ShadeFragment(vec2 texCoord, vec4 worldPos);

uniform bool u_ShowWires;

layout(location = 0) in vec2 i_TexCoord;
layout(location = 1) in vec4 i_WorldPos;
layout(location = 2) noperspective in vec3 i_Distance;

layout(location = 0) out vec4 o_FragColor;

void main()
{
    o_FragColor = ShadeFragment(i_TexCoord, i_WorldPos);

    if (u_ShowWires) {
        const float wireScale = 1.0; // scale of the wire in pixel
        vec4 wireColor = vec4(0.0, 0.75, 1.0, 1.0);
        vec3 distanceSquared = i_Distance * i_Distance;
        // TODO or vec3 distanceSquared = gl_BaryCoordEXT; when using
        // GL_EXT_fragment_shader_barycentric
        // See https://wunkolo.github.io/post/2022/07/gl_ext_fragment_shader_barycentric-wireframe
        float nearestDistance = min(min(distanceSquared.x, distanceSquared.y), distanceSquared.z);
        float blendFactor = exp2(-nearestDistance / wireScale);

        o_FragColor = mix(o_FragColor, wireColor, blendFactor);
    }
}
