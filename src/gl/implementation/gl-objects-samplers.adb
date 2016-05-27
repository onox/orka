--------------------------------------------------------------------------------
-- Copyright (c) 2016 onox <denkpadje@gmail.com>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with GL.API;
with GL.Helpers;
with GL.Low_Level;
with GL.Enums.Textures;

package body GL.Objects.Samplers is

   procedure Bind (Object : Sampler; Unit : Textures.Texture_Unit) is
   begin
      API.Bind_Sampler (UInt (Unit), Object.Reference.GL_Id);
      Raise_Exception_On_OpenGL_Error;
   end Bind;

   procedure Bind (Objects : Sampler_Array; First_Unit : Textures.Texture_Unit) is
      Sampler_Ids : Low_Level.UInt_Array (Objects'Range);
   begin
      for Index in Objects'Range loop
         Sampler_Ids (Index) := Objects (Index).Reference.GL_Id;
      end loop;
      API.Bind_Samplers (UInt (First_Unit), Sampler_Ids'Length, Sampler_Ids);
      Raise_Exception_On_OpenGL_Error;
   end Bind;

   overriding
   procedure Initialize_Id (Object : in out Sampler) is
      New_Id : UInt := 0;
   begin
      API.Create_Samplers (1, New_Id);
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := New_Id;
      Object.Reference.Initialized := True;
   end Initialize_Id;

   overriding
   procedure Delete_Id (Object : in out Sampler) is
   begin
      API.Delete_Samplers (1, (1 => Object.Reference.GL_Id));
      Raise_Exception_On_OpenGL_Error;
      Object.Reference.GL_Id := 0;
      Object.Reference.Initialized := False;
   end Delete_Id;

   -----------------------------------------------------------------------------
   --                           Sampler Parameters                            --
   -----------------------------------------------------------------------------

   procedure Set_Minifying_Filter (Object : Sampler;
                                   Filter : Minifying_Function) is
   begin
      API.Sampler_Parameter_Minifying_Function (Object.Reference.GL_Id,
                                                Enums.Textures.Min_Filter, Filter);
      Raise_Exception_On_OpenGL_Error;
   end Set_Minifying_Filter;

   function Minifying_Filter (Object : Sampler) return Minifying_Function is
      Ret : Minifying_Function := Minifying_Function'First;
   begin
      API.Get_Sampler_Parameter_Minifying_Function (Object.Reference.GL_Id,
                                                    Enums.Textures.Min_Filter, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Minifying_Filter;

   procedure Set_Magnifying_Filter (Object : Sampler;
                                    Filter : Magnifying_Function) is
   begin
      API.Sampler_Parameter_Magnifying_Function (Object.Reference.GL_Id,
                                                 Enums.Textures.Mag_Filter, Filter);
      Raise_Exception_On_OpenGL_Error;
   end Set_Magnifying_Filter;

   function Magnifying_Filter (Object : Sampler) return Magnifying_Function is
      Ret : Magnifying_Function := Magnifying_Function'First;
   begin
      API.Get_Sampler_Parameter_Magnifying_Function (Object.Reference.GL_Id,
                                                     Enums.Textures.Mag_Filter, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Magnifying_Filter;

   procedure Set_Minimum_LoD (Object : Sampler; Level : Double) is
   begin
      API.Sampler_Parameter_Float (Object.Reference.GL_Id,
                                   Enums.Textures.Min_LoD, Single (Level));
      Raise_Exception_On_OpenGL_Error;
   end Set_Minimum_LoD;

   function Minimum_LoD (Object : Sampler) return Double is
      Ret : Low_Level.Single_Array (1 .. 1);
   begin
      API.Get_Sampler_Parameter_Floats (Object.Reference.GL_Id,
                                        Enums.Textures.Min_LoD, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret (1));
   end Minimum_LoD;

   procedure Set_Maximum_LoD (Object : Sampler; Level : Double) is
   begin
      API.Sampler_Parameter_Float (Object.Reference.GL_Id,
                                   Enums.Textures.Max_LoD, Single (Level));
      Raise_Exception_On_OpenGL_Error;
   end Set_Maximum_LoD;

   function Maximum_LoD (Object : Sampler) return Double is
      Ret : Low_Level.Single_Array (1 .. 1);
   begin
      API.Get_Sampler_Parameter_Floats (Object.Reference.GL_Id,
                                        Enums.Textures.Max_LoD, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret (1));
   end Maximum_LoD;

   procedure Set_LoD_Bias (Object : Sampler; Level : Double) is
   begin
      API.Sampler_Parameter_Float (Object.Reference.GL_Id,
                                   Enums.Textures.LoD_Bias, Single (Level));
      Raise_Exception_On_OpenGL_Error;
   end Set_LoD_Bias;

   function LoD_Bias (Object : Sampler) return Double is
      Ret : Low_Level.Single_Array (1 .. 1);
   begin
      API.Get_Sampler_Parameter_Floats (Object.Reference.GL_Id,
                                        Enums.Textures.LoD_Bias, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret (1));
   end LoD_Bias;

   procedure Set_Max_Anisotropy (Object : Sampler; Degree : Double) is
   begin
      API.Sampler_Parameter_Float (Object.Reference.GL_Id,
                                   Enums.Textures.Max_Anisotropy,
                                   Single (Degree));
      Raise_Exception_On_OpenGL_Error;
   end Set_Max_Anisotropy;

   function Max_Anisotropy (Object : Sampler) return Double is
      Ret : Low_Level.Single_Array (1 .. 1);
   begin
      API.Get_Sampler_Parameter_Floats (Object.Reference.GL_Id,
                                        Enums.Textures.Max_Anisotropy, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Double (Ret (1));
   end Max_Anisotropy;

   procedure Set_X_Wrapping (Object : Sampler; Mode : Wrapping_Mode) is
   begin
      API.Sampler_Parameter_Wrapping_Mode (Object.Reference.GL_Id,
                                           Enums.Textures.Wrap_S, Mode);
      Raise_Exception_On_OpenGL_Error;
   end Set_X_Wrapping;

   function X_Wrapping (Object : Sampler) return Wrapping_Mode is
      Ret : Wrapping_Mode := Wrapping_Mode'First;
   begin
      API.Get_Sampler_Parameter_Wrapping_Mode (Object.Reference.GL_Id,
                                               Enums.Textures.Wrap_S, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end X_Wrapping;

   procedure Set_Y_Wrapping (Object : Sampler; Mode : Wrapping_Mode) is
   begin
      API.Sampler_Parameter_Wrapping_Mode (Object.Reference.GL_Id,
                                           Enums.Textures.Wrap_T, Mode);
      Raise_Exception_On_OpenGL_Error;
   end Set_Y_Wrapping;

   function Y_Wrapping (Object : Sampler) return Wrapping_Mode is
      Ret : Wrapping_Mode := Wrapping_Mode'First;
   begin
      API.Get_Sampler_Parameter_Wrapping_Mode (Object.Reference.GL_Id,
                                               Enums.Textures.Wrap_T, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Y_Wrapping;

   procedure Set_Z_Wrapping (Object : Sampler; Mode : Wrapping_Mode) is
   begin
      API.Sampler_Parameter_Wrapping_Mode (Object.Reference.GL_Id,
                                           Enums.Textures.Wrap_R, Mode);
      Raise_Exception_On_OpenGL_Error;
   end Set_Z_Wrapping;

   function Z_Wrapping (Object : Sampler) return Wrapping_Mode is
      Ret : Wrapping_Mode := Wrapping_Mode'First;
   begin
      API.Get_Sampler_Parameter_Wrapping_Mode (Object.Reference.GL_Id,
                                               Enums.Textures.Wrap_R, Ret);
      Raise_Exception_On_OpenGL_Error;
      return Ret;
   end Z_Wrapping;

   procedure Set_Border_Color (Object : Sampler; Color : Colors.Color) is
      Raw : constant Low_Level.Single_Array := Helpers.Float_Array (Color);
   begin
      API.Sampler_Parameter_Floats (Object.Reference.GL_Id,
                                    Enums.Textures.Border_Color, Raw);
      Raise_Exception_On_OpenGL_Error;
   end Set_Border_Color;

   function Border_Color (Object : Sampler) return Colors.Color is
      Raw : Low_Level.Single_Array (1 .. 4);
   begin
      API.Get_Sampler_Parameter_Floats (Object.Reference.GL_Id,
                                        Enums.Textures.Border_Color, Raw);
      Raise_Exception_On_OpenGL_Error;
      return Helpers.Color (Raw);
   end Border_Color;

   procedure Toggle_Compare_X_To_Texture (Object : Sampler; Enabled : Boolean) is
      Value : Enums.Textures.Compare_Kind;
   begin
      if Enabled then
         Value := Enums.Textures.Compare_R_To_Texture;
      else
         Value := Enums.Textures.None;
      end if;
      API.Sampler_Parameter_Compare_Kind (Object.Reference.GL_Id,
                                          Enums.Textures.Compare_Mode, Value);
      Raise_Exception_On_OpenGL_Error;
   end Toggle_Compare_X_To_Texture;

   function Compare_X_To_Texture_Enabled (Object : Sampler) return Boolean is
      use type Enums.Textures.Compare_Kind;

      Value : Enums.Textures.Compare_Kind := Enums.Textures.Compare_Kind'First;
   begin
      API.Get_Sampler_Parameter_Compare_Kind (Object.Reference.GL_Id,
                                              Enums.Textures.Compare_Mode, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value = Enums.Textures.Compare_R_To_Texture;
   end Compare_X_To_Texture_Enabled;

   procedure Set_Compare_Function (Object : Sampler; Func : Compare_Function) is
   begin
      API.Sampler_Parameter_Compare_Function (Object.Reference.GL_Id,
                                              Enums.Textures.Compare_Func, Func);
      Raise_Exception_On_OpenGL_Error;
   end Set_Compare_Function;

   function Current_Compare_Function (Object : Sampler) return Compare_Function is
      Value : Compare_Function := Compare_Function'First;
   begin
      API.Get_Sampler_Parameter_Compare_Function (Object.Reference.GL_Id,
                                                  Enums.Textures.Compare_Func, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Current_Compare_Function;

end GL.Objects.Samplers;
