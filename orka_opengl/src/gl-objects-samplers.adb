--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with GL.API;
with GL.Low_Level;
with GL.Enums.Textures;

package body GL.Objects.Samplers is

   procedure Bind (Object : Sampler; Unit : Textures.Texture_Unit) is
   begin
      API.Bind_Sampler.Ref (UInt (Unit), Object.Reference.GL_Id);
   end Bind;

   procedure Bind (Objects : Sampler_Array; First_Unit : Textures.Texture_Unit) is
      Sampler_Ids : Low_Level.UInt_Array (Objects'Range);
   begin
      for Index in Objects'Range loop
         Sampler_Ids (Index) := Objects (Index).Reference.GL_Id;
      end loop;
      API.Bind_Samplers.Ref (UInt (First_Unit), Sampler_Ids'Length, Sampler_Ids);
   end Bind;

   overriding
   procedure Initialize_Id (Object : in out Sampler) is
      New_Id : UInt := 0;
   begin
      API.Create_Samplers.Ref (1, New_Id);
      Object.Reference.GL_Id := New_Id;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Sampler) is
   begin
      API.Delete_Samplers.Ref (1, [Object.Reference.GL_Id]);
      Object.Reference.GL_Id := 0;
   end Delete_Id;

   -----------------------------------------------------------------------------
   --                           Sampler Parameters                            --
   -----------------------------------------------------------------------------

   procedure Set_Minifying_Filter (Object : Sampler;
                                   Filter : Minifying_Function) is
   begin
      API.Sampler_Parameter_Minifying_Function.Ref
        (Object.Reference.GL_Id, Enums.Textures.Min_Filter, Filter);
   end Set_Minifying_Filter;

   procedure Set_Magnifying_Filter (Object : Sampler;
                                    Filter : Magnifying_Function) is
   begin
      API.Sampler_Parameter_Magnifying_Function.Ref
        (Object.Reference.GL_Id, Enums.Textures.Mag_Filter, Filter);
   end Set_Magnifying_Filter;

   procedure Set_Minimum_LoD (Object : Sampler; Level : Double) is
   begin
      API.Sampler_Parameter_Float.Ref
        (Object.Reference.GL_Id, Enums.Textures.Min_LoD, Single (Level));
   end Set_Minimum_LoD;

   procedure Set_Maximum_LoD (Object : Sampler; Level : Double) is
   begin
      API.Sampler_Parameter_Float.Ref
        (Object.Reference.GL_Id, Enums.Textures.Max_LoD, Single (Level));
   end Set_Maximum_LoD;

   procedure Set_LoD_Bias (Object : Sampler; Level : Double) is
   begin
      API.Sampler_Parameter_Float.Ref
        (Object.Reference.GL_Id, Enums.Textures.LoD_Bias, Single (Level));
   end Set_LoD_Bias;

   procedure Set_Max_Anisotropy (Object : Sampler; Degree : Double) is
   begin
      API.Sampler_Parameter_Float.Ref
        (Object.Reference.GL_Id, Enums.Textures.Max_Anisotropy, Single (Degree));
   end Set_Max_Anisotropy;

   procedure Set_X_Wrapping (Object : Sampler; Mode : Wrapping_Mode) is
   begin
      API.Sampler_Parameter_Wrapping_Mode.Ref
        (Object.Reference.GL_Id, Enums.Textures.Wrap_S, Mode);
   end Set_X_Wrapping;

   procedure Set_Y_Wrapping (Object : Sampler; Mode : Wrapping_Mode) is
   begin
      API.Sampler_Parameter_Wrapping_Mode.Ref
        (Object.Reference.GL_Id, Enums.Textures.Wrap_T, Mode);
   end Set_Y_Wrapping;

   procedure Set_Z_Wrapping (Object : Sampler; Mode : Wrapping_Mode) is
   begin
      API.Sampler_Parameter_Wrapping_Mode.Ref
        (Object.Reference.GL_Id, Enums.Textures.Wrap_R, Mode);
   end Set_Z_Wrapping;

   use all type Colors.Border_Color;

   subtype GL_Color is Low_Level.Single_Array (1 .. 4);

   Vulkan_To_OpenGL : constant array (Colors.Border_Color) of GL_Color
     := [Transparent_Black => [0.0, 0.0, 0.0, 0.0],
         Opaque_Black      => [0.0, 0.0, 0.0, 1.0],
         Opaque_White      => [1.0, 1.0, 1.0, 1.0]];

   procedure Set_Border_Color (Object : Sampler; Color : Colors.Border_Color) is
      Raw : constant Low_Level.Single_Array := Vulkan_To_OpenGL (Color);
   begin
      API.Sampler_Parameter_Floats.Ref
        (Object.Reference.GL_Id, Enums.Textures.Border_Color, Raw);
   end Set_Border_Color;

   procedure Set_Compare_X_To_Texture (Object : Sampler; Enabled : Boolean) is
      use all type Enums.Textures.Compare_Kind;
   begin
      API.Sampler_Parameter_Compare_Kind.Ref
        (Object.Reference.GL_Id, Enums.Textures.Compare_Mode,
         (if Enabled then Compare_R_To_Texture else None));
   end Set_Compare_X_To_Texture;

   procedure Set_Compare_Function (Object : Sampler; Func : Compare_Function) is
   begin
      API.Sampler_Parameter_Compare_Function.Ref
        (Object.Reference.GL_Id, Enums.Textures.Compare_Func, Func);
   end Set_Compare_Function;

end GL.Objects.Samplers;
