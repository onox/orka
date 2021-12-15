#version 420 core

//  SPDX-License-Identifier: Apache-2.0

layout(origin_upper_left) in vec4 gl_FragCoord;

flat in uint var_InstanceID;

in VS_OUT {
    vec3 N;
    vec3 L;
    vec3 V;
    vec2 uv;
} fs_in;

layout(binding = 0) uniform sampler2DArray diffuseTexture;

out vec4 fs_out;

const float pi = 3.14159265358979323846264338327950288419716939937511;

struct Light {
    vec3 albedo;
    float radius;
    float intensity;
};

struct Material {
    float metallic;
    float roughness;
};

const Light light = {
    vec3(1.0, 1.0, 1.0),
    10.0,
    1.0,
};

const Material material = {
    0.2,
    0.7,
};

const vec3 dielectricSpecular = vec3(0.04, 0.04, 0.04);

// Equations from Epic [3] at SIGGRAPH '13 course [4]

// Microfacet distribution function
vec3 D_GGX(in float nh)
{
    // alpha = roughness^2
    const float alpha = material.roughness * material.roughness;

    const float alpha2 = alpha * alpha;
    const float denom = nh * nh * (alpha2 - 1.0) + 1.0;

    return vec3(alpha2 / (pi * denom * denom));
}

// Geometric attenuation factor
vec3 G_Schlick(in float nl, in float nv)
{
    // Remap roughness to reduce `hotness': roughness := (roughness + 1) / 2
    // k = alpha / 2 where alpha = roughness^2
    const float rpo = (material.roughness + 1.0);
    const float k = (rpo * rpo) / 8.0;

    const float G1l = nl / (nl * (1.0 - k) + k);
    const float G1v = nv / (nv * (1.0 - k) + k);

    return vec3(G1l * G1v);
}

// Fresnel reflection coefficient
vec3 F_Schlick(const in float vh, in vec4 baseColor)
{
    // Specular reflectance at normal incidence
    const vec3 F0 = mix(dielectricSpecular, baseColor.xyz, material.metallic);

    // Spherical Gaussian approximation instead of power (slightly more efficient)
    return F0 + (vec3(1.0) - F0) * exp2((-5.55473 * vh - 6.98316) * vh);
//    return F0 + (vec3(1.0) - F0) * pow(1.0 - vh, 5);
}

vec3 diffuse(in vec3 n, in vec3 l, in vec4 baseColor)
{
    const vec3 cdiff = mix(baseColor.xyz * (1.0 - dielectricSpecular.r), vec3(0.0), material.metallic);
    const float nl = max(dot(n, l), 0.0); // n.l factor

    return cdiff * nl / pi;
}

vec3 specular(in vec3 n, in vec3 l, in vec3 v, in vec4 baseColor)
{
    const vec3 h = normalize(v + l);

    const float nl = max(dot(n, l), 0.0); // n.l factor
    const float nv = max(dot(n, v), 0.0); // n.v factor
    const float nh = max(dot(n, h), 0.0); // n.h factor

    const float vh = max(dot(v, h), 0.0); // v.h factor

    const float denom = 4.0 * nl * nv;

    const vec3 f = F_Schlick(vh, baseColor);
    const vec3 g = G_Schlick(nl, nv);
    const vec3 d = D_GGX(nh);

    return (f * g * d) / denom;
}

// Inverse square falloff
float falloff(in float distance, in float lightRadius)
{
    // saturate(1 - (distance / lightRadius)^4)^2
    // ------------------------------------------
    //              distance^2 + 1
    const float sat = clamp(1.0 - pow(distance / lightRadius, 4.0), 0.0, 1.0);
    return (sat * sat) / (distance * distance + 1.0);
}

void main(void) {
    const vec3 normal    = normalize(fs_in.N);
    const vec3 light_dir = normalize(fs_in.L);
    const vec3 view_dir  = normalize(fs_in.V);

    const vec4 albedo = texture(diffuseTexture, vec3(fs_in.uv, var_InstanceID));

    const vec3 fd = diffuse(normal, light_dir, albedo);
    const vec3 fs = specular(normal, light_dir, view_dir, albedo);

    // Clamp light distance to a maximum distance so that the object is always visible
    const float light_distance = min(3.0, length(fs_in.L));

    const float fo = light.intensity * falloff(light_distance, light.radius);
    fs_out = vec4(vec3(fo) * light.albedo * (fd + fs), 1.0);
}

// References:
//
// [1] https://www.youtube.com/watch?v=j-A0mwsJRmk
// [2] http://blog.selfshadow.com/publications/s2013-shading-course/hoffman/s2013_pbs_physics_math_notes.pdf
// [3] http://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf
// [4] http://blog.selfshadow.com/publications/s2013-shading-course/
