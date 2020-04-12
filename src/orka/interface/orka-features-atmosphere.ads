--  SPDX-License-Identifier: BSD-3-Clause
--
--  Copyright (c) 2017 Eric Bruneton
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--  1. Redistributions of source code must retain the above copyright
--     notice, this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution.
--  3. Neither the name of the copyright holders nor the names of its
--     contributors may be used to endorse or promote products derived from
--     this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
--  THE POSSIBILITY OF SUCH DAMAGE.

--  The shader returned by Get_Shader provides the following
--  functions (that you need to forward declare in your own shaders to be able to
--  compile them separately):
--
--     // Returns the radiance of the Sun, outside the atmosphere.
--     vec3 GetSolarRadiance();
--
--     // Returns the sky radiance along the segment from 'camera' to the nearest
--     // atmosphere boundary in direction 'view_ray', as well as the transmittance
--     // along this segment.
--     vec3 GetSkyRadiance(vec3 camera, vec3 view_ray, double shadow_length,
--         vec3 sun_direction, out vec3 transmittance, out bool intersects_ground);
--
--     // Returns the sky radiance along the segment from 'camera' to 'p', as well as
--     // the transmittance along this segment.
--     vec3 GetSkyRadianceToPoint(vec3 camera, vec3 p, double shadow_length,
--         vec3 sun_direction, out vec3 transmittance);
--
--     // Returns the sun and sky irradiance received on a surface patch located at 'p'
--     // and whose normal vector is 'normal'.
--     vec3 GetSunAndSkyIrradiance(vec3 p, vec3 normal, vec3 sun_direction,
--         out vec3 sky_irradiance);
--
--     // Returns the luminance of the Sun, outside the atmosphere.
--     vec3 GetSolarLuminance();
--
--     // Returns the sky luminance along the segment from 'camera' to the nearest
--     // atmosphere boundary in direction 'view_ray', as well as the transmittance
--     // along this segment.
--     vec3 GetSkyLuminance(vec3 camera, vec3 view_ray, double shadow_length,
--         vec3 sun_direction, out vec3 transmittance, out bool intersects_ground);
--
--     // Returns the sky luminance along the segment from 'camera' to 'p', as well as
--     // the transmittance along this segment.
--     vec3 GetSkyLuminanceToPoint(vec3 camera, vec3 p, double shadow_length,
--         vec3 sun_direction, out vec3 transmittance);
--
--     // Returns the sun and sky illuminance received on a surface patch located at
--     // 'p' and whose normal vector is 'normal'.
--     vec3 GetSunAndSkyIlluminance(vec3 p, vec3 normal, vec3 sun_direction,
--         out vec3 sky_illuminance);
--
--  where
--
--  - camera and p must be expressed in a reference frame where the planet center
--    is at the origin, and measured in the unit passed to the constructor's
--    length_unit_in_meters argument. camera can be in space, but p must be
--    inside the atmosphere
--
--  - view_ray, sun_direction and normal are unit direction vectors expressed
--    in the same reference frame (with sun_direction pointing *towards* the Sun)
--
--  - shadow_length is the length along the segment which is in shadow, measured
--    in the unit passed to the constructor's length_unit_in_meters argument
--
--  and where
--
--  - the first 4 functions return spectral radiance and irradiance values
--    (in $W.m^{-2}.sr^{-1}.nm^{-1}$ and $W.m^{-2}.nm^{-1}$), at the 3 wavelengths
--    K_Lambda_R, K_Lambda_G, K_Lambda_B (in this order)
--
--  - the other functions return luminance and illuminance values (in
--    $cd.m^{-2}$ and $lx$) in linear [sRGB](https://en.wikipedia.org/wiki/SRGB)
--    space (i.e. before adjustments for gamma correction)
--
--  - all the functions return the (unitless) transmittance of the atmosphere
--    along the specified segment at the 3 wavelengths K_Lambda_R,
--    K_Lambda_G, K_Lambda_B (in this order)
--
--  Note: The precomputed atmosphere textures can store either irradiance
--  or illuminance values (see the Num_Precomputed_Wavelengths parameter):
--
--  - when using irradiance values, the RGB channels of these textures contain
--    spectral irradiance values, in $W.m^{-2}.nm^{-1}$, at the 3 wavelengths
--    K_Lambda_R, K_Lambda_G, K_Lambda_B (in this order). The API functions
--    returning radiance values return these precomputed values (times the
--    phase functions), while the API functions returning luminance values use
--    the approximation described in
--    "A Qualitative and Quantitative Evaluation of 8 Clear Sky Models" [1],
--    section 14.3, to convert 3 radiance values to linear sRGB luminance values
--
--  - when using illuminance values, the RGB channels of these textures contain
--    illuminance values, in $lx$, in linear sRGB space. These illuminance values
--    are precomputed as described in
--    "Real-time Spectral Scattering in Large-scale Natural Participating Media" [2],
--    section 4.4 (i.e. Num_Precomputed_Wavelengths irradiance values are
--    precomputed, and then converted to sRGB via a numerical integration of this
--    spectrum with the CIE color matching functions). The API functions returning
--    luminance values return these precomputed values (times the phase functions),
--    while *the API functions returning radiance values are not provided*
--
--  [1] https://arxiv.org/pdf/1612.04336.pdf
--  [2] http://www.oskee.wz.cz/stranka/uploads/SCCG10ElekKmoch.pdf

private with GL.Low_Level.Enums;
private with GL.Objects.Textures;
private with GL.Objects.Samplers;

with Ada.Containers.Vectors;

with GL.Types;

with Orka.Resources.Locations;
with Orka.Rendering.Programs.Modules;

package Orka.Features.Atmosphere is
   pragma Preelaborate;

   type Luminance_Type is (None, Approximate, Precomputed);

   use GL.Types;

   type Density_Profile_Layer is record
      Width, Exp_Term, Exp_Scale, Linear_Term, Constant_Term : Double := 0.0;
   end record;
   --  An atmosphere layer of Width (in m), and whose density is defined as
   --  Exp_Term * exp(Exp_Scale * h) + Linear_Term * h + Constant_Term,
   --  clamped to [0,1], and where h is the altitude (in m). Exp_Term and
   --  Constant_Term are unitless, while Exp_Scale and Linear_Term are in m^-1.

   use type Ada.Containers.Count_Type;

   package Double_Vectors  is new Ada.Containers.Vectors (Natural, Double);
   package Density_Vectors is new Ada.Containers.Vectors (Natural, Density_Profile_Layer);

   type Model_Data (Samples : Ada.Containers.Count_Type) is record
      Luminance : Luminance_Type;

      Wavelengths : Double_Vectors.Vector;
      --  The wavelength values, in nanometers, and sorted in increasing order, for
      --  which the solar_irradiance, rayleigh_scattering, mie_scattering,
      --  mie_extinction and ground_albedo samples are provided. If your shaders
      --  use luminance values (as opposed to radiance values, see above), use a
      --  large number of wavelengths (e.g. between 15 and 50) to get accurate
      --  results (this number of wavelengths has absolutely no impact on the
      --  shader performance).

      Solar_Irradiance : Double_Vectors.Vector;
      --  Solar irradiance at the top of the atmosphere, in W/m^2/nm. This
      --  vector must have the same size as the wavelengths parameter.

      Sun_Angular_Radius : Double;
      --  Sun's angular radius in radians. Warning: the implementation uses
      --  approximations that are valid only if this value is smaller than 0.1.

      Bottom_Radius : Double;
      --  Distance between the planet center and the bottom of the atmosphere, in m

      Top_Radius : Double;
      --  Distance between the planet center and the top of the atmosphere, in m

      Rayleigh_Density : Density_Vectors.Vector;
      --  The density profile of air molecules, i.e. a function from altitude to
      --  dimensionless values between 0 (null density) and 1 (maximum density).
      --  Layers must be sorted from bottom to top. The width of the last layer is
      --  ignored, i.e. it always extend to the top atmosphere boundary. At most 2
      --  layers can be specified.

      Rayleigh_Scattering : Double_Vectors.Vector;
      --  The scattering coefficient of air molecules at the altitude where their
      --  density is maximum (usually the bottom of the atmosphere), as a function
      --  of wavelength, in m^-1. The scattering coefficient at altitude h is equal
      --  to 'rayleigh_scattering' times 'rayleigh_density' at this altitude. This
      --  vector must have the same size as the wavelengths parameter.

      Mie_Density : Density_Vectors.Vector;
      --  The density profile of aerosols, i.e. a function from altitude to
      --  dimensionless values between 0 (null density) and 1 (maximum density).
      --  Layers must be sorted from bottom to top. The width of the last layer is
      --  ignored, i.e. it always extend to the top atmosphere boundary. At most 2
      --  layers can be specified.

      Mie_Scattering : Double_Vectors.Vector;
      --  The scattering coefficient of aerosols at the altitude where their
      --  density is maximum (usually the bottom of the atmosphere), as a function
      --  of wavelength, in m^-1. The scattering coefficient at altitude h is equal
      --  to 'mie_scattering' times 'mie_density' at this altitude. This vector
      --  must have the same size as the wavelengths parameter.

      Mie_Extinction : Double_Vectors.Vector;
      --  The extinction coefficient of aerosols at the altitude where their
      --  density is maximum (usually the bottom of the atmosphere), as a function
      --  of wavelength, in m^-1. The extinction coefficient at altitude h is equal
      --  to 'mie_extinction' times 'mie_density' at this altitude. This vector
      --  must have the same size as the wavelengths parameter.

      Mie_Phase_Function_G : Double;
      --  The asymetry parameter for the Cornette-Shanks phase function for the
      --  aerosols.

      Absorption_Density : Density_Vectors.Vector;
      --  The density profile of air molecules that absorb light (e.g. ozone), i.e.
      --  a function from altitude to dimensionless values between 0 (null density)
      --  and 1 (maximum density). Layers must be sorted from bottom to top. The
      --  width of the last layer is ignored, i.e. it always extend to the top
      --  atmosphere boundary. At most 2 layers can be specified.

      Absorption_Extinction : Double_Vectors.Vector;
      --  The extinction coefficient of molecules that absorb light (e.g. ozone) at
      --  the altitude where their density is maximum, as a function of wavelength,
      --  in m^-1. The extinction coefficient at altitude h is equal to
      --  'absorption_extinction' times 'absorption_density' at this altitude. This
      --  vector must have the same size as the wavelengths parameter.

      Ground_Albedo : Double_Vectors.Vector;
      --  The average albedo of the ground, as a function of wavelength. This
      --  vector must have the same size as the wavelengths parameter.

      Max_Sun_Zenith_Angle : Double;
      --  The maximum Sun zenith angle for which atmospheric scattering must be
      --  precomputed, in radians (for maximum precision, use the smallest Sun
      --  zenith angle yielding negligible sky light radiance values. For instance,
      --  for the Earth case, 102 degrees is a good choice for most cases (120
      --  degrees is necessary for very high exposure values).

      Length_Unit_In_Meters : Double;
      --  The length unit used in your shaders and meshes. This is the length unit
      --  which must be used when calling the atmosphere model shader functions.

      Num_Precomputed_Wavelengths : GL.Types.UInt;
      --  The number of wavelengths for which atmospheric scattering must be
      --  precomputed (the temporary GPU memory used during precomputations, and
      --  the GPU memory used by the precomputed results, is independent of this
      --  number, but the precomputation time is directly proportional to this
      --  number):
      --  - if this number is less than or equal to 3, scattering is precomputed
      --  for 3 wavelengths, and stored as irradiance values. Then both the
      --  radiance-based and the luminance-based API functions are provided (see
      --  the above note).
      --  - otherwise, scattering is precomputed for this number of wavelengths
      --  (rounded up to a multiple of 3), integrated with the CIE color matching
      --  functions, and stored as illuminance values. Then only the
      --  luminance-based API functions are provided (see the above note).

      Combine_Scattering_Textures : Boolean;
      --  Whether to pack the (red component of the) single Mie scattering with the
      --  Rayleigh and multiple scattering in a single texture, or to store the
      --  (3 components of the) single Mie scattering in a separate texture.

      Half_Precision : Boolean;
      --  Whether to use half precision floats (16 bits) or single precision floats
      --  (32 bits) for the precomputed textures. Half precision is sufficient for
      --  most cases, except for very high exposure values.
   end record
     with Dynamic_Predicate =>
           Model_Data.Rayleigh_Density.Length   <= 2
       and Model_Data.Absorption_Density.Length <= 2
       and Model_Data.Sun_Angular_Radius < 0.1
       and Model_Data.Wavelengths.Length           = Model_Data.Samples
       and Model_Data.Solar_Irradiance.Length      = Model_Data.Samples
       and Model_Data.Rayleigh_Scattering.Length   = Model_Data.Samples
       and Model_Data.Mie_Scattering.Length        = Model_Data.Samples
       and Model_Data.Mie_Extinction.Length        = Model_Data.Samples
       and Model_Data.Absorption_Extinction.Length = Model_Data.Samples
       and Model_Data.Ground_Albedo.Length         = Model_Data.Samples;

   type Precomputed_Textures is private;

   procedure Bind_Textures (Object : Precomputed_Textures);

   type Model (Data : not null access constant Model_Data) is tagged limited private;

   function Create_Model
     (Data     : not null access constant Model_Data;
      Location : Resources.Locations.Location_Ptr) return Model;

   function Compute_Textures (Object : Model; Scattering_Orders : Natural := 4)
     return Precomputed_Textures;

   function Get_Shader (Object : Model) return Rendering.Programs.Modules.Module;

   procedure Convert_Spectrum_To_Linear_SRGB (Data : Model_Data; R, G, B : out Double);
   --  Utility method to convert a function of the wavelength to linear sRGB
   --
   --  Wavelengths and solar irradiance must have the same size. The integral of
   --  Spectrum times each CIE_2_Deg_Color_Matching_Functions (and times
   --  Max_Luminous_Efficacy) is computed to get XYZ values, which are then
   --  converted to linear sRGB with the XYZ_To_SRGB matrix.
   --
   --  For white balance, divide R, G, and B by the average of the three numbers

private

   package Textures renames GL.Objects.Textures;
   package LE renames GL.Low_Level.Enums;

   type Precomputed_Textures is record
      Sampler : GL.Objects.Samplers.Sampler;
      Combine_Scattering : Boolean;

      Transmittance_Texture : Textures.Texture (LE.Texture_2D);
      Scattering_Texture    : Textures.Texture (LE.Texture_3D);
      Irradiance_Texture    : Textures.Texture (LE.Texture_2D);

      Optional_Single_Mie_Scattering_Texture : Textures.Texture (LE.Texture_3D);
      --  Unused if Combine_Scattering is True
   end record;

   type Model (Data : not null access constant Model_Data) is tagged limited record
      Data_Definitions : Resources.Byte_Array_Pointers.Pointer;
      Data_Functions   : Resources.Byte_Array_Pointers.Pointer;

      Location : Resources.Locations.Location_Access;

      Sky_K_R, Sky_K_G, Sky_K_B : GL.Types.Double;
      Sun_K_R, Sun_K_G, Sun_K_B : GL.Types.Double;
   end record;

   function Create_Sampler return GL.Objects.Samplers.Sampler;

   K_Lambda_Min : constant Double := 360.0;
   K_Lambda_Max : constant Double := 830.0;

end Orka.Features.Atmosphere;
