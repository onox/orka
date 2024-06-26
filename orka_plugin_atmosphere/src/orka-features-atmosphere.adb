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

--  This package's role is to precompute the transmittance, scattering and
--  irradiance textures. The GLSL functions to precompute them are provided
--  in functions.frag, but they are not sufficient.
--  They must be used in fully functional shaders and programs, and these
--  programs must be called in the correct order, with the correct input and
--  output textures (via framebuffer objects), to precompute each scattering
--  order in sequence, as described in Algorithm 4.1 of our paper [1].
--
--  [1] https://hal.inria.fr/inria-00288758/en

with Ada.Characters.Latin_1;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Unbounded;

with GL.Blending;
with GL.Buffers;
with GL.Pixels;
with GL.Toggles;
with GL.Types;

with Orka.Features.Atmosphere.Constants;
with Orka.Rendering.Drawing;
with Orka.Rendering.Framebuffers;
with Orka.Rendering.Shaders.Objects;
with Orka.Rendering.Shaders.Uniforms;
with Orka.Registry;
with Orka.Resources.Locations;
with Orka.Types;

package body Orka.Features.Atmosphere is

   package Framebuffers renames Orka.Rendering.Framebuffers;
   package SU renames Ada.Strings.Unbounded;

   K_Lambda_R : constant Float_64 := 680.0;
   K_Lambda_G : constant Float_64 := 550.0;
   K_Lambda_B : constant Float_64 := 440.0;

   function Convert (Bytes : Resources.Byte_Array) return String renames Resources.Convert;

   use Orka.Features.Atmosphere.Constants;

   use all type Orka.Rendering.Samplers.Wrapping_Mode;
   use all type Orka.Rendering.Samplers.Minifying_Function;
   use all type Orka.Rendering.Samplers.Magnifying_Function;

   function Create_Sampler return Orka.Rendering.Samplers.Sampler is
     (Orka.Rendering.Samplers.Create_Sampler
        ((Wrapping          => [others => Clamp_To_Edge],
          Minifying_Filter  => Linear,
          Magnifying_Filter => Linear,
          others            => <>)));

   --  16F precision for the transmittance gives artifacts
   function Create_Texture (Width, Height : Size) return Rendering.Textures.Texture is
     (Orka.Rendering.Textures.Create_Texture
        ((Kind    => LE.Texture_2D,
          Compressed => False,
          Format  => GL.Pixels.RGBA32F,
          Size    => [Width, Height, 1],
          Levels  => 1,
          Samples => 1)));

   function Create_Texture
     (Width, Height, Depth : Size;
      Half_Precision : Boolean) return Rendering.Textures.Texture
   is (Orka.Rendering.Textures.Create_Texture
        ((Kind    => LE.Texture_3D,
          Compressed => False,
          Format  => (if Half_Precision then GL.Pixels.RGBA16F else GL.Pixels.RGBA32F),
          Size    => [Width, Height, Depth],
          Levels  => 1,
          Samples => 1)));

   -----------------------------------------------------------------------------

   type Enable_Blend_Array is array (GL.Buffers.Draw_Buffer_Index range <>) of Boolean;

   procedure Draw_Quad (Blend : Enable_Blend_Array) is
   begin
      for Index in Blend'Range loop
         if Blend (Index) then
            GL.Toggles.Enable (GL.Toggles.Blend, Index);
         end if;
      end loop;

      Orka.Rendering.Drawing.Draw (GL.Types.Triangles, 0, 3);

      for Index in Blend'Range loop
         GL.Toggles.Disable (GL.Toggles.Blend, Index);
      end loop;
   end Draw_Quad;

   -----------------------------------------------------------------------------

   --  Finally, we need a utility function to compute the value of the
   --  conversion constants *_RADIANCE_TO_LUMINANCE, used in the shader
   --  to convert the spectral results into luminance values. These are
   --  the constants k_r, k_g, k_b described in Section 14.3 of
   --  "A Qualitative and Quantitative Evaluation of 8 Clear Sky Models" [1].

   --  Computing their value requires an integral of a function times a
   --  CIE color matching function. Thus, we first need functions to
   --  interpolate an arbitrary function (specified by some samples), and
   --  a CIE color matching function (specified by tabulated values), at
   --  an arbitrary wavelength. This is the purpose of the two functions
   --  below.
   --
   --  [1] https://arxiv.org/pdf/1612.04336.pdf

   function CIE_Color_Matching_Function_Table_Value
     (Wavelength : Float_64;
      Column     : Size) return Float_64 is
   begin
      if Wavelength <= K_Lambda_Min or Wavelength >= K_Lambda_Max then
         return 0.0;
      end if;

      declare
         U   : Float_64 := (Wavelength - K_Lambda_Min) / 5.0;
         Row : constant Size := Integer_32 (Float_64'Floor (U));
         pragma Assert (Row + 1 < 95);

         pragma Assert (CIE_2_Deg_Color_Matching_Functions (4 * Row) <= Wavelength);
         pragma Assert (CIE_2_Deg_Color_Matching_Functions (4 * (Row + 1)) >= Wavelength);
      begin
         U := U - Float_64 (Row);
         return CIE_2_Deg_Color_Matching_Functions (4 * Row + Column) * (1.0 - U) +
           CIE_2_Deg_Color_Matching_Functions (4 * (Row + 1) + Column) * U;
      end;
   end CIE_Color_Matching_Function_Table_Value;

   function Interpolate
     (Wavelengths, Functions : Double_Vectors.Vector;
      Wavelength : Float_64) return Float_64
   is
      pragma Assert (Wavelengths.Length = Functions.Length);
   begin
      if Wavelength < Wavelengths.First_Element then
         return Functions.First_Element;
      end if;

      for Index in Wavelengths.First_Index .. Wavelengths.Last_Index - 1 loop
         if Wavelength < Wavelengths (Index + 1) then
            declare
               U : constant Float_64 := (Wavelength - Wavelengths (Index))
                 / (Wavelengths (Index + 1) - Wavelengths (Index));
            begin
               return Functions (Index) * (1.0 - U) + Functions (Index + 1) * U;
            end;
         end if;
      end loop;
      return Functions.Last_Element;
   end Interpolate;

   --  We can then implement a utility function to compute the "spectral
   --  radiance to luminance" conversion constants (see Section 14.3 in
   --  "A Qualitative and Quantitative Evaluation of 8 Clear Sky Models" [1]
   --  for their definitions).
   --
   --  [1] https://arxiv.org/pdf/1612.04336.pdf

   procedure Compute_Spectral_Radiance_To_Luminance_Factors
     (Wavelengths, Solar_Irradiance : Double_Vectors.Vector;
      Lambda_Power : Float_64;
      K_R, K_G, K_B : out Float_64)  --  The returned constants are in lumen.nm / watt
   is
      package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_64);
      use EF;

      Solar_R : constant Float_64 := Interpolate (Wavelengths, Solar_Irradiance, K_Lambda_R);
      Solar_G : constant Float_64 := Interpolate (Wavelengths, Solar_Irradiance, K_Lambda_G);
      Solar_B : constant Float_64 := Interpolate (Wavelengths, Solar_Irradiance, K_Lambda_B);

      D_Lambda : constant Float_64 := 1.0;
      Lambda   : Float_64 := K_Lambda_Min;
   begin
      K_R := 0.0;
      K_G := 0.0;
      K_B := 0.0;

      while Lambda < K_Lambda_Max loop
         declare
            X_Bar : constant Float_64 := CIE_Color_Matching_Function_Table_Value (Lambda, 1);
            Y_Bar : constant Float_64 := CIE_Color_Matching_Function_Table_Value (Lambda, 2);
            Z_Bar : constant Float_64 := CIE_Color_Matching_Function_Table_Value (Lambda, 3);

            R_Bar : constant Float_64
              := XYZ_To_SRGB (0) * X_Bar + XYZ_To_SRGB (1) * Y_Bar + XYZ_To_SRGB (2) * Z_Bar;
            G_Bar : constant Float_64
              := XYZ_To_SRGB (3) * X_Bar + XYZ_To_SRGB (4) * Y_Bar + XYZ_To_SRGB (5) * Z_Bar;
            B_Bar : constant Float_64
              := XYZ_To_SRGB (6) * X_Bar + XYZ_To_SRGB (7) * Y_Bar + XYZ_To_SRGB (8) * Z_Bar;

            Irradiance : constant Float_64 := Interpolate (Wavelengths, Solar_Irradiance, Lambda);
         begin
            K_R := K_R + R_Bar * Irradiance / Solar_R * (Lambda / K_Lambda_R) ** Lambda_Power;
            K_G := K_G + G_Bar * Irradiance / Solar_G * (Lambda / K_Lambda_G) ** Lambda_Power;
            K_B := K_B + B_Bar * Irradiance / Solar_B * (Lambda / K_Lambda_B) ** Lambda_Power;
         end;

         Lambda := Lambda + D_Lambda;
      end loop;

      K_R := K_R * Max_Luminous_Efficacy * D_Lambda;
      K_G := K_G * Max_Luminous_Efficacy * D_Lambda;
      K_B := K_B * Max_Luminous_Efficacy * D_Lambda;
   end Compute_Spectral_Radiance_To_Luminance_Factors;

   --  A function that creates a shader header containing our atmosphere computation
   --  functions, specialized for the given atmosphere parameters and for the 3
   --  wavelengths in Lambdas

   function Shader_Header (Result : Model; Lambdas : Float_64_Array) return String is
      package EF is new Ada.Numerics.Generic_Elementary_Functions (Float_64);
      use Ada.Characters.Latin_1;

      Data : not null access constant Model_Data renames Result.Data;

      function To_String
        (V       : Double_Vectors.Vector;
         Scale   : Float_64) return String
      is
         R : constant Float_64 := Interpolate (Data.Wavelengths, V, Lambdas (0)) * Scale;
         G : constant Float_64 := Interpolate (Data.Wavelengths, V, Lambdas (1)) * Scale;
         B : constant Float_64 := Interpolate (Data.Wavelengths, V, Lambdas (2)) * Scale;
      begin
         return "vec3(" & R'Image & "," & G'Image & "," & B'Image & ")";
      end To_String;

      function Density_Layer (Layer : Density_Profile_Layer) return String is
         Width : constant Float_64 := Layer.Width / Data.Length_Unit_In_Meters;
         Scale : constant Float_64 := Layer.Exp_Scale * Data.Length_Unit_In_Meters;
         Linear_Term : constant Float_64 := Layer.Linear_Term * Data.Length_Unit_In_Meters;
      begin
         return "DensityProfileLayer(" &
           Width'Image & "," & Layer.Exp_Term'Image & "," &
           Scale'Image & "," & Linear_Term'Image & "," &
           Layer.Constant_Term'Image & ")";
      end Density_Layer;

      function Density_Profile (Layers : Density_Vectors.Vector) return String is
         K_Layer_Count : constant := 2;

         K_Layers   : Density_Vectors.Vector := Layers.Copy;
         Zero_Layer : Density_Profile_Layer;

         Result     : SU.Unbounded_String;
      begin
         while K_Layers.Length < K_Layer_Count loop
            K_Layers.Prepend (Zero_Layer);
         end loop;
         SU.Append (Result, "DensityProfile(DensityProfileLayer[" &
           Natural'Image (K_Layer_Count) & "](");
         for Index in K_Layers.First_Index .. K_Layers.Last_Index - 1 loop
            SU.Append (Result, Density_Layer (K_Layers (Index)) & ",");
         end loop;
         SU.Append (Result, Density_Layer (K_Layers.Last_Element) & "))");
         return SU.To_String (Result);
      end Density_Profile;

      Definitions_GLSL : constant String
        := Convert (Resources.Byte_Array'(Result.Data_Definitions.Get));
      Functions_GLSL   : constant String
        := Convert (Resources.Byte_Array'(Result.Data_Functions.Get));

      Bottom_Radius : constant Float_64 := Data.Bottom_Radius / Data.Length_Unit_In_Meters;
      Top_Radius    : constant Float_64 := Data.Top_Radius / Data.Length_Unit_In_Meters;
   begin
      return
        "#version 420" & LF &
        "#extension GL_ARB_fragment_layer_viewport : require" & LF &
        "#define IN(x) const in x" & LF &
        "#define OUT(x) out x" & LF &
        "#define TEMPLATE(x)" & LF &
        "#define TEMPLATE_ARGUMENT(x)" & LF &
        "#define assert(x)" & LF &
        "const int TRANSMITTANCE_TEXTURE_WIDTH = " &
          Natural'Image (Transmittance_Texture_Width) & ";" & LF &
        "const int TRANSMITTANCE_TEXTURE_HEIGHT = " &
          Natural'Image (Transmittance_Texture_Height) & ";" & LF &
        "const int SCATTERING_TEXTURE_R_SIZE = " &
          Natural'Image (Scattering_Texture_R_Size) & ";" & LF &
        "const int SCATTERING_TEXTURE_MU_SIZE = " &
          Natural'Image (Scattering_Texture_Mu_Size) & ";" & LF &
        "const int SCATTERING_TEXTURE_MU_S_SIZE = " &
          Natural'Image (Scattering_Texture_Mu_S_Size) & ";" & LF &
        "const int SCATTERING_TEXTURE_NU_SIZE = " &
          Natural'Image (Scattering_Texture_Nu_Size) & ";" & LF &
        "const int IRRADIANCE_TEXTURE_WIDTH = " &
          Natural'Image (Irradiance_Texture_Width) & ";" & LF &
        "const int IRRADIANCE_TEXTURE_HEIGHT = " &
          Natural'Image (Irradiance_Texture_Height) & ";" & LF &
        (if Data.Combine_Scattering_Textures then
           "#define COMBINED_SCATTERING_TEXTURES" & LF
         else "") &
        Definitions_GLSL & LF &
        "const AtmosphereParameters ATMOSPHERE = AtmosphereParameters(" & LF &
          To_String (Data.Solar_Irradiance, 1.0) & "," & LF &
          Data.Sun_Angular_Radius'Image & "," & LF &
          Bottom_Radius'Image & "," & LF &
          Top_Radius'Image & "," & LF &
          Density_Profile (Data.Rayleigh_Density) & "," & LF &
          To_String
            (Data.Rayleigh_Scattering, Data.Length_Unit_In_Meters) & "," & LF &
          Density_Profile (Data.Mie_Density) & "," & LF &
          To_String
            (Data.Mie_Scattering, Data.Length_Unit_In_Meters) & "," & LF &
          To_String
            (Data.Mie_Extinction, Data.Length_Unit_In_Meters) & "," & LF &
          Data.Mie_Phase_Function_G'Image & "," & LF &
          Density_Profile (Data.Absorption_Density) & "," & LF &
          To_String
            (Data.Absorption_Extinction, Data.Length_Unit_In_Meters) & "," & LF &
          To_String (Data.Ground_Albedo, 1.0) & "," & LF &
          EF.Cos (Data.Max_Sun_Zenith_Angle)'Image & ");" & LF &
        "const vec3 SKY_SPECTRAL_RADIANCE_TO_LUMINANCE = vec3(" &
          Result.Sky_K_R'Image & "," &
          Result.Sky_K_G'Image & "," &
          Result.Sky_K_B'Image & ");" & LF &
        "const vec3 SUN_SPECTRAL_RADIANCE_TO_LUMINANCE = vec3(" &
          Result.Sun_K_R'Image & "," &
          Result.Sun_K_G'Image & "," &
          Result.Sun_K_B'Image & ");" & LF &
        Functions_GLSL & LF;
   end Shader_Header;

   -----------------------------------------------------------------------------

   function Create_Model
     (Context : aliased Orka.Contexts.Context'Class;
      Data    : aliased Model_Data) return Model
   is
      --  Compute the values for the SKY_RADIANCE_TO_LUMINANCE constant. In theory
      --  this should be 1 in precomputed illuminance mode (because the precomputed
      --  textures already contain illuminance values). In practice, however, storing
      --  true illuminance values in half precision textures yields artefacts
      --  (because the values are too large), so we store illuminance values divided
      --  by MAX_LUMINOUS_EFFICACY instead. This is why, in precomputed illuminance
      --  mode, we set SKY_RADIANCE_TO_LUMINANCE to MAX_LUMINOUS_EFFICACY.
      Precompute_Illuminance : constant Boolean := Data.Num_Precomputed_Wavelengths > 3;

      Power_Sky : constant Float_64 := -3.0;
      Power_Sun : constant Float_64 :=  0.0;

      Location : constant Orka.Resources.Locations.Location_Ptr := Orka.Registry.Location ("orka-atmosphere");
   begin
      return Result : Model (Context'Access, Data'Access) do
         if Precompute_Illuminance then
            Result.Sky_K_R := Max_Luminous_Efficacy;
            Result.Sky_K_G := Max_Luminous_Efficacy;
            Result.Sky_K_B := Max_Luminous_Efficacy;
         else
            Compute_Spectral_Radiance_To_Luminance_Factors
              (Data.Wavelengths, Data.Solar_Irradiance, Power_Sky,
               Result.Sky_K_R, Result.Sky_K_G, Result.Sky_K_B);
         end if;

         --  Compute the values for the SUN_RADIANCE_TO_LUMINANCE constant
         Compute_Spectral_Radiance_To_Luminance_Factors
           (Data.Wavelengths, Data.Solar_Irradiance, Power_Sun,
            Result.Sun_K_R, Result.Sun_K_G, Result.Sun_K_B);

         Result.Data_Definitions := Location.Read_Data ("definitions.frag");
         Result.Data_Functions   := Location.Read_Data ("functions.frag");
      end return;
   end Create_Model;

   --  The utility method is implemented with a simple numerical integration
   --  of the given function, times the CIE color matching functions (with an
   --  integration step of 1 nm), followed by a matrix multiplication
   procedure Convert_Spectrum_To_Linear_SRGB (Data : Model_Data; R, G, B : out Float_64) is
      D_Lambda : constant Float_64 := 1.0;

      Lambda  : Float_64 := K_Lambda_Min;
      X, Y, Z : Float_64 := 0.0;
   begin
      while Lambda < K_Lambda_Max loop
         declare
            Value : constant Float_64 :=
              Interpolate (Data.Wavelengths, Data.Solar_Irradiance, Lambda);
         begin
            X := X + CIE_Color_Matching_Function_Table_Value (Lambda, 1) * Value;
            Y := Y + CIE_Color_Matching_Function_Table_Value (Lambda, 2) * Value;
            Z := Z + CIE_Color_Matching_Function_Table_Value (Lambda, 3) * Value;
         end;

         Lambda := Lambda + D_Lambda;
      end loop;
      R := Max_Luminous_Efficacy *
        (XYZ_To_SRGB (0) * X + XYZ_To_SRGB (1) * Y + XYZ_To_SRGB (2) * Z) * D_Lambda;
      G := Max_Luminous_Efficacy *
        (XYZ_To_SRGB (3) * X + XYZ_To_SRGB (4) * Y + XYZ_To_SRGB (5) * Z) * D_Lambda;
      B := Max_Luminous_Efficacy *
        (XYZ_To_SRGB (6) * X + XYZ_To_SRGB (7) * Y + XYZ_To_SRGB (8) * Z) * D_Lambda;
   end Convert_Spectrum_To_Linear_SRGB;

   --  Implementation of the precomputation algorithm described in
   --  Algorithm 4.1 of [1]. Each step is explained by the inline
   --  comments below.
   --
   --  [1] https://hal.inria.fr/inria-00288758/en
   procedure Precompute
     (Object : Model;
      Result : Precomputed_Textures;
      Delta_Irradiance_Texture, Delta_Rayleigh_Scattering_Texture,
      Delta_Mie_Scattering_Texture, Delta_Scattering_Density_Texture,
      Delta_Multiple_Scattering_Texture : Rendering.Textures.Texture;
      Lambdas : Float_64_Array;
      Luminance_From_Radiance : Float_32_Array;
      Blend : Boolean;
      Num_Scattering_Orders : Natural)
   is
      LR : Float_32_Array renames Luminance_From_Radiance;

      Luminance_From_Radiance_Mat3 : constant Orka.Types.Singles.Matrix4 :=
        [[LR (0), LR (1), LR (2), 0.0],
         [LR (3), LR (4), LR (5), 0.0],
         [LR (6), LR (7), LR (8), 0.0],
         [0.0, 0.0, 0.0, 0.0]];

      use Orka.Rendering.Shaders;
      use Orka.Rendering.Shaders.Objects;

      --  The precomputations require specific GLSL programs for each
      --  precomputation step
      Program_VS : constant Optional_Shader :=
        Create_Shader (Vertex_Shader, "orka-atmosphere:oversized-triangle.vert");

      Program_GS : constant Optional_Shader :=
        Create_Shader (Geometry_Shader, "orka-atmosphere:layer.geom");

      Location : constant Orka.Resources.Locations.Location_Ptr := Orka.Registry.Location ("orka-atmosphere");

      Header : constant String := Object.Shader_Header (Lambdas);

      FS_Transmittance : constant String
        := Convert (Resources.Byte_Array'(Location.Read_Data
             ("compute-transmittance.frag").Get));

      FS_Direct_Irradiance : constant String
        := Convert (Resources.Byte_Array'(Location.Read_Data
             ("compute-direct-irradiance.frag").Get));

      FS_Indirect_Irradiance : constant String
        := Convert (Resources.Byte_Array'(Location.Read_Data
             ("compute-indirect-irradiance.frag").Get));

      FS_Single_Scattering : constant String
        := Convert (Resources.Byte_Array'(Location.Read_Data
             ("compute-single-scattering.frag").Get));

      FS_Multiple_Scattering : constant String
        := Convert (Resources.Byte_Array'(Location.Read_Data
             ("compute-multiple-scattering.frag").Get));

      FS_Scattering_Density : constant String
        := Convert (Resources.Byte_Array'(Location.Read_Data
             ("compute-scattering-density.frag").Get));

      Program_Transmittance : constant Shader_Objects :=
         [Vertex_Shader   => Program_VS,
          Fragment_Shader => Create_Shader_From_Source (Fragment_Shader, Header & FS_Transmittance),
          others          => Empty];

      Program_Direct_Irradiance : constant Shader_Objects :=
         [Vertex_Shader   => Program_VS,
          Fragment_Shader => Create_Shader_From_Source (Fragment_Shader, Header & FS_Direct_Irradiance),
          others          => Empty];

      Program_Indirect_Irradiance : constant Shader_Objects :=
         [Vertex_Shader   => Program_VS,
          Fragment_Shader => Create_Shader_From_Source (Fragment_Shader, Header & FS_Indirect_Irradiance),
          others          => Empty];

      Program_Single_Scattering : constant Shader_Objects :=
         [Vertex_Shader   => Program_VS,
          Geometry_Shader => Program_GS,
          Fragment_Shader => Create_Shader_From_Source (Fragment_Shader, Header & FS_Single_Scattering),
          others          => Empty];

      Program_Multiple_Scattering : constant Shader_Objects :=
         [Vertex_Shader   => Program_VS,
          Geometry_Shader => Program_GS,
          Fragment_Shader => Create_Shader_From_Source (Fragment_Shader, Header & FS_Multiple_Scattering),
          others          => Empty];

      Program_Scattering_Density : constant Shader_Objects :=
         [Vertex_Shader   => Program_VS,
          Geometry_Shader => Program_GS,
          Fragment_Shader => Create_Shader_From_Source (Fragment_Shader, Header & FS_Scattering_Density),
          others          => Empty];

      -------------------------------------------------------------------------

      FBO_Transmittance : Framebuffers.Framebuffer := Framebuffers.Create_Framebuffer
        (Width  => Constants.Transmittance_Texture_Width,
         Height => Constants.Transmittance_Texture_Height);

      FBO_Irradiance : Framebuffers.Framebuffer := Framebuffers.Create_Framebuffer
        (Width  => Constants.Irradiance_Texture_Width,
         Height => Constants.Irradiance_Texture_Height);

      FBO_Scattering : Framebuffers.Framebuffer := Framebuffers.Create_Framebuffer
        (Width  => Constants.Scattering_Texture_Width,
         Height => Constants.Scattering_Texture_Height);

      use all type GL.Blending.Equation;
      use all type GL.Blending.Blend_Factor;
      use all type Framebuffers.Color_Attachment_Point;
   begin
      GL.Blending.Set_Blend_Equation ((Func_Add, Func_Add));
      GL.Blending.Set_Blend_Func ((One, One, One, One));

      Result.Transmittance_Texture.Bind (0);
      --  transmittance_texture
      Delta_Rayleigh_Scattering_Texture.Bind (1);
      --  single_rayleigh_scattering_texture

      Delta_Mie_Scattering_Texture.Bind (2);
      --  single_mie_scattering_texture
      Delta_Multiple_Scattering_Texture.Bind (3);
      --  multiple_scattering_texture
      Delta_Irradiance_Texture.Bind (4);
      --  irradiance_texture

      Delta_Scattering_Density_Texture.Bind (5);
      --  scattering_density_texture

      --  Compute the transmittance and store it in Transmittance_Texture
      FBO_Transmittance.Use_Framebuffer;
      FBO_Transmittance.Attach (Result.Transmittance_Texture, Color_Attachment_0);
      FBO_Transmittance.Set_Draw_Buffers
        ([0 => GL.Buffers.Color_Attachment0]);

      Object.Context.Bind_Shaders (Program_Transmittance);
      Draw_Quad ([1 .. 0 => <>]);

      -------------------------------------------------------------------------

      --  Compute the direct irradiance, store it in Delta_Irradiance_Texture and,
      --  depending on 'blend', either initialize Irradiance_Texture with
      --  zeros or leave it unchanged (we don't want the irradiance in
      --  Irradiance_Texture, but only the irradiance from the sky
      FBO_Irradiance.Use_Framebuffer;
      FBO_Irradiance.Attach (Delta_Irradiance_Texture, Color_Attachment_0);
      FBO_Irradiance.Attach (Result.Irradiance_Texture, Color_Attachment_1);
      FBO_Irradiance.Set_Draw_Buffers
        ([0 => GL.Buffers.Color_Attachment0,
          1 => GL.Buffers.Color_Attachment1]);

      Object.Context.Bind_Shaders (Program_Direct_Irradiance);
      Draw_Quad ([False, Blend]);

      -------------------------------------------------------------------------

      --  Compute the rayleigh and mie single scattering, store them in
      --  Delta_Rayleigh_Scattering_Texture and Delta_Mie_Scattering_Texture,
      --  and either them or accumulate them in Scattering_Texture and
      --  Optional_Single_Mie_Scattering_Texture
      FBO_Scattering.Use_Framebuffer;
      FBO_Scattering.Attach (Delta_Rayleigh_Scattering_Texture, Color_Attachment_0);
      FBO_Scattering.Attach (Delta_Mie_Scattering_Texture, Color_Attachment_1);
      FBO_Scattering.Attach (Result.Scattering_Texture, Color_Attachment_2);

      if not Object.Data.Combine_Scattering_Textures then
         FBO_Scattering.Attach (Result.Optional_Single_Mie_Scattering_Texture, Color_Attachment_3);
         FBO_Scattering.Set_Draw_Buffers
           ([0 => GL.Buffers.Color_Attachment0,
             1 => GL.Buffers.Color_Attachment1,
             2 => GL.Buffers.Color_Attachment2,
             3 => GL.Buffers.Color_Attachment3]);
      else
         FBO_Scattering.Set_Draw_Buffers
           ([0 => GL.Buffers.Color_Attachment0,
             1 => GL.Buffers.Color_Attachment1,
             2 => GL.Buffers.Color_Attachment2]);
      end if;

      Object.Context.Bind_Shaders (Program_Single_Scattering);
      Program_Single_Scattering (Fragment_Shader).Value.Uniform ("luminance_from_radiance").Set_Matrix
        (Luminance_From_Radiance_Mat3);
      for Layer in 0 .. Integer_32 (Constants.Scattering_Texture_Depth - 1) loop
         Program_Single_Scattering (Fragment_Shader).Value.Uniform ("layer").Set_Int (Layer);
         Draw_Quad ([False, False, Blend, Blend]);
      end loop;

      -------------------------------------------------------------------------

      --  Compute the 2nd, 3rd and 4th order of scattering, in sequence
      for Scattering_Order in 2 .. Integer_32 (Num_Scattering_Orders) loop
         --  Compute the scattering density, and store it in
         --  Delta_Scattering_Density_Texture
         FBO_Scattering.Use_Framebuffer;
         FBO_Scattering.Attach (Delta_Scattering_Density_Texture, Color_Attachment_0);
         FBO_Scattering.Detach (Color_Attachment_1);
         FBO_Scattering.Detach (Color_Attachment_2);
         FBO_Scattering.Detach (Color_Attachment_3);
         FBO_Scattering.Set_Draw_Buffers
           ([0 => GL.Buffers.Color_Attachment0]);

         Object.Context.Bind_Shaders (Program_Scattering_Density);
         Program_Scattering_Density (Fragment_Shader).Value.Uniform ("scattering_order").Set_Int (Scattering_Order);
         for Layer in 0 .. Integer_32 (Constants.Scattering_Texture_Depth - 1) loop
            Program_Scattering_Density (Fragment_Shader).Value.Uniform ("layer").Set_Int (Layer);
            Draw_Quad ([1 .. 0 => <>]);
         end loop;

         --  Compute the indirect irradiance, store it in Delta_Irradiance_Texture
         --  and accumulate it in Irradiance_Texture
         FBO_Irradiance.Use_Framebuffer;
         FBO_Irradiance.Attach (Delta_Irradiance_Texture, Color_Attachment_0);
         FBO_Irradiance.Attach (Result.Irradiance_Texture, Color_Attachment_1);
         FBO_Irradiance.Set_Draw_Buffers
           ([0 => GL.Buffers.Color_Attachment0,
             1 => GL.Buffers.Color_Attachment1]);

         Object.Context.Bind_Shaders (Program_Indirect_Irradiance);
         Program_Indirect_Irradiance (Fragment_Shader).Value.Uniform ("luminance_from_radiance").Set_Matrix
          (Luminance_From_Radiance_Mat3);
         Program_Indirect_Irradiance (Fragment_Shader).Value.Uniform ("scattering_order").Set_Int (Scattering_Order - 1);
         Draw_Quad ([False, True]);

         --  Compute the multiple scattering, store it in
         --  Delta_Multiple_Scattering_Texture, and accumulate it in
         --  Scattering_Texture
         FBO_Scattering.Use_Framebuffer;
         FBO_Scattering.Attach (Delta_Multiple_Scattering_Texture, Color_Attachment_0);
         FBO_Scattering.Attach (Result.Scattering_Texture, Color_Attachment_1);
         FBO_Scattering.Set_Draw_Buffers
           ([0 => GL.Buffers.Color_Attachment0,
             1 => GL.Buffers.Color_Attachment1]);

         Object.Context.Bind_Shaders (Program_Multiple_Scattering);
         Program_Multiple_Scattering (Fragment_Shader).Value.Uniform ("luminance_from_radiance").Set_Matrix
          (Luminance_From_Radiance_Mat3);
         for Layer in 0 .. Integer_32 (Constants.Scattering_Texture_Depth - 1) loop
            Program_Multiple_Scattering (Fragment_Shader).Value.Uniform ("layer").Set_Int (Layer);
            Draw_Quad ([False, True]);
         end loop;
      end loop;
   end Precompute;

   --  Precomputes the atmosphere textures. It first allocates the
   --  temporary resources it needs and then calls Precompute to do the
   --  actual precomputations.
   --
   --  Note that there are two precomputation modes here, depending on whether we
   --  want to store precomputed irradiance or illuminance values:
   --
   --  - In precomputed irradiance mode, we simply need to call
   --    Precompute with the 3 wavelengths for which we want to precompute
   --    irradiance, namely K_Lambda_R, K_Lambda_G,
   --    K_Lambda_B (with the identity matrix for
   --    Luminance_From_Radiance, since we don't want any conversion from
   --    radiance to luminance)
   --
   --  - In precomputed illuminance mode, we need to precompute irradiance for
   --    Num_Precomputed_Wavelengths, and then integrate the results,
   --    multiplied with the 3 CIE xyz color matching functions and the XYZ to sRGB
   --    matrix to get sRGB illuminance values.
   --
   --    A naive solution would be to allocate temporary textures for the
   --    intermediate irradiance results, then perform the integration from irradiance
   --    to illuminance and store the result in the final precomputed texture. In
   --    pseudo-code (and assuming one wavelength per texture instead of 3):
   --
   --      create n temporary irradiance textures
   --      for each wavelength lambda in the n wavelengths:
   --         precompute irradiance at lambda into one of the temporary textures
   --      initializes the final illuminance texture with zeros
   --      for each wavelength lambda in the n wavelengths:
   --        accumulate in the final illuminance texture the product of the
   --        precomputed irradiance at lambda (read from the temporary textures)
   --        with the value of the 3 sRGB color matching functions at lambda (i.e.
   --        the product of the XYZ to sRGB matrix with the CIE xyz color matching
   --        functions).
   --
   --    However, this be would waste GPU memory. Instead, we can avoid allocating
   --    temporary irradiance textures, by merging the two above loops:
   --
   --      for each wavelength lambda in the n wavelengths:
   --        accumulate in the final illuminance texture (or, for the first
   --        iteration, set this texture to) the product of the precomputed
   --        irradiance at lambda (computed on the fly) with the value of the 3
   --        sRGB color matching functions at lambda.
   --
   --    This is the method we use below, with 3 wavelengths per iteration instead
   --    of 1, using Precompute to compute 3 irradiances values per
   --    iteration, and Luminance_From_Radiance to multiply 3 irradiances
   --    with the values of the 3 sRGB color matching functions at 3 different
   --    wavelengths (yielding a 3x3 matrix).
   function Compute_Textures (Object : Model; Scattering_Orders : Natural := 4)
     return Precomputed_Textures
   is
      --  The precomputations require temporary textures, in particular to store the
      --  contribution of one scattering order, which is needed to compute the next
      --  order of scattering (the final precomputed textures store the sum of all
      --  the scattering orders)
      Delta_Irradiance_Texture : constant Rendering.Textures.Texture (LE.Texture_2D)
        := Create_Texture (Constants.Irradiance_Texture_Width,
                           Constants.Irradiance_Texture_Height);
      Delta_Rayleigh_Scattering_Texture : constant Rendering.Textures.Texture (LE.Texture_3D)
        := Create_Texture (Constants.Scattering_Texture_Width,
                           Constants.Scattering_Texture_Height,
                           Constants.Scattering_Texture_Depth,
                           Object.Data.Half_Precision);
      Delta_Mie_Scattering_Texture : constant Rendering.Textures.Texture (LE.Texture_3D)
        := Create_Texture (Constants.Scattering_Texture_Width,
                           Constants.Scattering_Texture_Height,
                           Constants.Scattering_Texture_Depth,
                           Object.Data.Half_Precision);
      Delta_Scattering_Density_Texture : constant Rendering.Textures.Texture (LE.Texture_3D)
        := Create_Texture (Constants.Scattering_Texture_Width,
                           Constants.Scattering_Texture_Height,
                           Constants.Scattering_Texture_Depth,
                           Object.Data.Half_Precision);

      --  Delta_Multiple_Scattering_Texture is only needed to compute scattering
      --  order 3 or more, while Delta_Rayleigh_Scattering_Texture and
      --  Delta_Mie_Scattering_Texture are only needed to compute double scattering.
      --  Therefore, to save memory, we can store Delta_Rayleigh_Scattering_Texture
      --  and Delta_Multiple_Scattering_Texture in the same GPU texture.
      Delta_Multiple_Scattering_Texture : constant Rendering.Textures.Texture (LE.Texture_3D)
        := Delta_Rayleigh_Scattering_Texture;

      --  Allocate the precomputed textures before precomputing them
      Textures : Precomputed_Textures :=
        (Sampler            => Create_Sampler,
         Combine_Scattering => Object.Data.Combine_Scattering_Textures,
         Transmittance_Texture => Create_Texture
           (Transmittance_Texture_Width, Transmittance_Texture_Height),
         Irradiance_Texture => Create_Texture
           (Irradiance_Texture_Width, Irradiance_Texture_Height),
         Scattering_Texture => Create_Texture
           (Scattering_Texture_Width, Scattering_Texture_Height, Scattering_Texture_Depth,
            Object.Data.Half_Precision),
         others             => <>);
   begin
      Textures.Sampler.Bind (0);
      Textures.Sampler.Bind (1);
      Textures.Sampler.Bind (2);
      Textures.Sampler.Bind (3);

      if not Object.Data.Combine_Scattering_Textures then
         Textures.Optional_Single_Mie_Scattering_Texture := Create_Texture
           (Scattering_Texture_Width, Scattering_Texture_Height, Scattering_Texture_Depth,
            Object.Data.Half_Precision);
      end if;

      if Object.Data.Num_Precomputed_Wavelengths <= 3 then
         declare
            Lambdas : constant Float_64_Array := [K_Lambda_R, K_Lambda_G, K_Lambda_B];
            Luminance_From_Radiance : constant Float_32_Array
              := [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0];
         begin
            Object.Precompute
              (Textures,
               Delta_Irradiance_Texture, Delta_Rayleigh_Scattering_Texture,
               Delta_Mie_Scattering_Texture, Delta_Scattering_Density_Texture,
               Delta_Multiple_Scattering_Texture,
               Lambdas, Luminance_From_Radiance,
               Blend => False, Num_Scattering_Orders => Scattering_Orders);
         end;
      else
         declare
            Iterations : constant Unsigned_32 := (Object.Data.Num_Precomputed_Wavelengths + 2) / 3;
            D_Lambda : constant Float_64
              := (K_Lambda_Max - K_Lambda_Min) / Float_64 (3 * Iterations);
         begin
            for I in 0 .. Iterations - 1 loop
               declare
                  Lambdas : constant Float_64_Array :=
                    [K_Lambda_Min + (3.0 * Float_64 (I) + 0.5) * D_Lambda,
                     K_Lambda_Min + (3.0 * Float_64 (I) + 1.5) * D_Lambda,
                     K_Lambda_Min + (3.0 * Float_64 (I) + 2.5) * D_Lambda];

                  function Coeff (Lambda : Float_64; Component : Size) return Float_32 is
                     --  Note that we don't include MAX_LUMINOUS_EFFICACY here, to avoid
                     --  artefacts due to too large values when using half precision on GPU.
                     --  We add this term back in atmosphere.frag, via
                     --  SKY_SPECTRAL_RADIANCE_TO_LUMINANCE (see also the comments in the
                     --  Model constructor).
                     X : constant Float_64 := CIE_Color_Matching_Function_Table_Value (Lambda, 1);
                     Y : constant Float_64 := CIE_Color_Matching_Function_Table_Value (Lambda, 2);
                     Z : constant Float_64 := CIE_Color_Matching_Function_Table_Value (Lambda, 3);
                  begin
                     return Float_32
                      ((XYZ_To_SRGB (Component * 3 + 0) * X +
                        XYZ_To_SRGB (Component * 3 + 1) * Y +
                        XYZ_To_SRGB (Component * 3 + 2) * Z) * D_Lambda);
                  end Coeff;

                  Luminance_From_Radiance : constant Float_32_Array :=
                    [Coeff (Lambdas (0), 0), Coeff (Lambdas (0), 1), Coeff (Lambdas (0), 2),
                     Coeff (Lambdas (1), 0), Coeff (Lambdas (1), 1), Coeff (Lambdas (1), 2),
                     Coeff (Lambdas (2), 0), Coeff (Lambdas (2), 1), Coeff (Lambdas (2), 2)];
               begin
                  Object.Precompute
                    (Textures,
                     Delta_Irradiance_Texture, Delta_Rayleigh_Scattering_Texture,
                     Delta_Mie_Scattering_Texture, Delta_Scattering_Density_Texture,
                     Delta_Multiple_Scattering_Texture,
                     Lambdas, Luminance_From_Radiance,
                     Blend => I > 0, Num_Scattering_Orders => Scattering_Orders);
               end;
            end loop;
         end;

         --  After the above iterations, the transmittance texture contains the
         --  transmittance for the 3 wavelengths used at the last iteration. But we
         --  want the transmittance at kLambdaR, kLambdaG, kLambdaB instead, so we
         --  must recompute it here for these 3 wavelengths:
         declare
            Lambdas : constant Float_64_Array := [K_Lambda_R, K_Lambda_G, K_Lambda_B];

            use Orka.Rendering.Shaders;
            use Orka.Rendering.Shaders.Objects;

            Location : constant Orka.Resources.Locations.Location_Ptr := Orka.Registry.Location ("orka-atmosphere");

            Header : constant String := Object.Shader_Header (Lambdas);

            FS_Transmittance : constant String
              := Convert (Resources.Byte_Array'(Location.Read_Data
                ("compute-transmittance.frag").Get));

            Program_Transmittance : constant Shader_Objects :=
              [Vertex_Shader   => Create_Shader (Vertex_Shader, "orka-atmosphere:oversized-triangle.vert"),
               Fragment_Shader => Create_Shader_From_Source (Fragment_Shader, Header & FS_Transmittance),
               others          => Empty];

            FBO_Transmittance : Framebuffers.Framebuffer := Framebuffers.Create_Framebuffer
              (Width  => Constants.Transmittance_Texture_Width,
               Height => Constants.Transmittance_Texture_Height);
         begin
            FBO_Transmittance.Use_Framebuffer;
            FBO_Transmittance.Attach (Textures.Transmittance_Texture);
            FBO_Transmittance.Set_Draw_Buffers
              ([0 => GL.Buffers.Color_Attachment0]);

            Object.Context.Bind_Shaders (Program_Transmittance);
            Draw_Quad ([1 .. 0 => <>]);
         end;
      end if;

      return Textures;
   end Compute_Textures;

   function Get_Shader (Object : Model) return Rendering.Shaders.Modules.Shader_Module is
      Precompute_Illuminance : constant Boolean := Object.Data.Num_Precomputed_Wavelengths > 3;

      Location : constant Orka.Resources.Locations.Location_Ptr := Orka.Registry.Location ("orka-atmosphere");

      Atmosphere_Fragment_Shader : constant String := Convert
        (Resources.Byte_Array'(Location.Read_Data ("atmosphere.frag").Get));

      use Ada.Characters.Latin_1;

      --  Create and compile the fragment shader providing our API.
      --
      --  It defines an "ATMOSPHERE" constant containing the atmosphere
      --  parameters (we use constants instead of uniforms to enable
      --  constant folding and propagation optimizations in the GLSL compiler),
      --  concatenated with functions.frag, and with Atmosphere_Fragment_Shader, to
      --  create the fragment shader
      Shader_Source : constant String
        := Object.Shader_Header ([K_Lambda_R, K_Lambda_G, K_Lambda_B]) &
           (if Precompute_Illuminance then "" else "#define RADIANCE_API_ENABLED" & LF) &
           Atmosphere_Fragment_Shader;

      use all type Rendering.Shaders.Shader_Kind;
   begin
      return Rendering.Shaders.Modules.Create_Module_From_Source (Fragment_Shader, Shader_Source);
   end Get_Shader;

   procedure Bind_Textures (Object : Precomputed_Textures) is
   begin
      Object.Sampler.Bind (0);
      Object.Sampler.Bind (1);
      Object.Sampler.Bind (2);
      Object.Sampler.Bind (3);

      Object.Transmittance_Texture.Bind (0);
      Object.Scattering_Texture.Bind (1);

      --  Only used by GetSunAndSkyIrradiance to compute radiance
      --  reflected by the ground
      Object.Irradiance_Texture.Bind (2);

      if not Object.Combine_Scattering then
         Object.Optional_Single_Mie_Scattering_Texture.Bind (3);
      end if;
   end Bind_Textures;

end Orka.Features.Atmosphere;
