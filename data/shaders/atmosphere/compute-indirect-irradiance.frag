//  SPDX-License-Identifier: BSD-3-Clause

/**
 * Copyright (c) 2017 Eric Bruneton
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

layout(location = 0) out vec3 delta_irradiance;
layout(location = 1) out vec3 irradiance;

// Used in precomputed illuminance mode to convert the radiance values
// computed by the functions in functions.glsl to luminance values
uniform mat3 luminance_from_radiance;

layout(binding = 1) uniform sampler3D single_rayleigh_scattering_texture;
layout(binding = 2) uniform sampler3D single_mie_scattering_texture;
layout(binding = 3) uniform sampler3D multiple_scattering_texture;
uniform int scattering_order;

void main(void) {
    delta_irradiance = ComputeIndirectIrradianceTexture(
        ATMOSPHERE, single_rayleigh_scattering_texture,
        single_mie_scattering_texture, multiple_scattering_texture,
        gl_FragCoord.xy, scattering_order);
    irradiance = luminance_from_radiance * delta_irradiance;
}
