#version 420 core

#extension GL_ARB_enhanced_layouts : require

// SPDX-License-Identifier: Apache-2.0
//
// Copyright (c) 2019 onox <denkpadje@gmail.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#define NOSE_SIDES 16

layout(lines) in;
layout(triangle_strip, max_vertices = 4 * NOSE_SIDES + 4) out;

uniform mat4 proj;
uniform uvec2 size;
uniform vec4 axisLength;

in vec4 vs_color[];

out vec4 gs_color;

const float PI = 3.141592653589793;
const float sidesNose = NOSE_SIDES;

mat3 rotate(in const vec3 axis, in const float angle) {
    const float ca = cos(angle);
    const float sa = sin(angle);

    const float mca = 1.0 - ca;

    const vec3 mcar = mca * axis.xxy * axis.yzz;
    const vec3 rsa = axis * sa;

    return mat3(
        vec3(ca + mca * axis.x*axis.x, mcar.x + rsa.z, mcar.y - rsa.y),
        vec3(mcar.x - rsa.z, ca + mca * axis.y*axis.y, mcar.z + rsa.x),
        vec3(mcar.y + rsa.y, mcar.z - rsa.x, ca + mca * axis.z*axis.z)
    );
}

void main() {
    const vec2 v0 = (proj * gl_in[0].gl_Position).xy;
    const vec2 v1 = (proj * gl_in[1].gl_Position).xy;

    const vec2 mainEdge = v1 - v0;

    const float edgeMainAngle = atan(mainEdge.y, mainEdge.x);
    const vec2 edgeCrossDir = normalize(vec2(
        cos(edgeMainAngle - 0.5 * PI),
        sin(edgeMainAngle - 0.5 * PI)
    ));

    const float lengthEdgeCross = (0.5 * axisLength.x) / length(0.5 * size * edgeCrossDir);

    const vec2 v00 = v0 + (edgeCrossDir * +lengthEdgeCross);
    const vec2 v01 = v0 + (edgeCrossDir * -lengthEdgeCross);

    const vec2 v10 = v1 + (edgeCrossDir * +lengthEdgeCross);
    const vec2 v11 = v1 + (edgeCrossDir * -lengthEdgeCross);

    gl_Position = vec4(v00, 0.0, 1.0);
    gs_color = vs_color[0];
    EmitVertex();

    gl_Position = vec4(v10, 0.0, 1.0);
    gs_color = vs_color[1];
    EmitVertex();

    gl_Position = vec4(v01, 0.0, 1.0);
    gs_color = vs_color[0];
    EmitVertex();

    gl_Position = vec4(v11, 0.0, 1.0);
    gs_color = vs_color[1];
    EmitVertex();

    EndPrimitive();

    //////////////////////////////////////////////////////////////////////

    const vec3 screenV0 = gl_in[0].gl_Position.xyz;
    const vec3 screenV1 = gl_in[1].gl_Position.xyz;

    // screenAxis is axisLength.y pixels long
    vec3 screenAxis = screenV1 - screenV0;
    vec3 screenSide = length(screenAxis) * normalize(cross(screenAxis.zxy, screenAxis));

    screenAxis *= axisLength.w / axisLength.y;
    screenSide *= (axisLength.z * 0.5) / axisLength.y;

    const vec3 screenNose = screenAxis + screenV1;

    gs_color = vs_color[1];

    const float angleNoseSide = PI * 2.0 / sidesNose;
    const mat3 rotateAngle = rotate(normalize(screenAxis), angleNoseSide);

    for (int i = 0; i < sidesNose; i++) {
        gl_Position = proj * vec4(screenNose, 1.0);
        gl_Position.zw = vec2(0.0, 1.0);
        EmitVertex();

        gl_Position = proj * vec4(screenV1 + screenSide, 1.0);
        gl_Position.zw = vec2(0.0, 1.0);
        EmitVertex();

        screenSide = rotateAngle * screenSide;
        gl_Position = proj * vec4(screenV1 + screenSide, 1.0);
        gl_Position.zw = vec2(0.0, 1.0);
        EmitVertex();

        gl_Position = proj * vec4(screenV1, 1.0);
        gl_Position.zw = vec2(0.0, 1.0);
        EmitVertex();

        EndPrimitive();
    }
}
