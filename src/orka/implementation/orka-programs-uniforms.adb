--  Copyright (c) 2017 onox <denkpadje@gmail.com>
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

with Ada.Unchecked_Conversion;

package body Orka.Programs.Uniforms is

   Sampler_Kind_Array : constant array (LE.Texture_Kind) of LE.Resource_Type :=
     (LE.Texture_1D                   => LE.Sampler_1D,
      LE.Texture_2D                   => LE.Sampler_2D,
      LE.Texture_3D                   => LE.Sampler_3D,
      LE.Texture_Rectangle            => LE.Sampler_2D_Rect,
      LE.Texture_Cube_Map             => LE.Sampler_Cube,
      LE.Texture_1D_Array             => LE.Sampler_1D_Array,
      LE.Texture_2D_Array             => LE.Sampler_2D_Array,
      LE.Texture_Buffer               => LE.Sampler_Buffer,
      LE.Texture_Cube_Map_Array       => LE.Sampler_3D,
      LE.Texture_2D_Multisample       => LE.Sampler_2D_Multisample,
      LE.Texture_2D_Multisample_Array => LE.Sampler_2D_Multisample_Array);

   procedure Set_Matrix (Object : Uniform; Value : TS.Matrix4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => TS.Matrix4, Target => GL.Types.Singles.Matrix4);
   begin
      Object.GL_Uniform.Set_Single_Matrix (Convert (Value));
   end Set_Matrix;

   procedure Set_Matrix (Object : Uniform; Value : TD.Matrix4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => TD.Matrix4, Target => GL.Types.Doubles.Matrix4);
   begin
      Object.GL_Uniform.Set_Double_Matrix (Convert (Value));
   end Set_Matrix;

   procedure Set_Vector (Object : Uniform; Value : TS.Vector4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => TS.Vector4, Target => GL.Types.Singles.Vector4);
   begin
      Object.GL_Uniform.Set_Single_Vector (Convert (Value));
   end Set_Vector;

   procedure Set_Vector (Object : Uniform; Value : TD.Vector4) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => TD.Vector4, Target => GL.Types.Doubles.Vector4);
   begin
      Object.GL_Uniform.Set_Double_Vector (Convert (Value));
   end Set_Vector;

   procedure Set_Single (Object : Uniform; Value : GL.Types.Single) is
   begin
      Object.GL_Uniform.Set_Single (Value);
   end Set_Single;

   procedure Set_Double (Object : Uniform; Value : GL.Types.Double) is
   begin
      Object.GL_Uniform.Set_Double (Value);
   end Set_Double;

   procedure Set_Int (Object : Uniform; Value : GL.Types.Int) is
   begin
      Object.GL_Uniform.Set_Int (Value);
   end Set_Int;

   procedure Set_UInt (Object : Uniform; Value : GL.Types.UInt) is
   begin
      Object.GL_Uniform.Set_UInt (Value);
   end Set_UInt;

   procedure Set_Texture
     (Object  : Uniform_Sampler;
      Texture : GL.Objects.Textures.Texture'Class;
      Binding : GL.Types.Int) is
   begin
      Object.GL_Uniform.Set_Int (Binding);
      Texture.Bind_Texture_Unit (Binding);
   end Set_Texture;

   function Create_Uniform_Sampler (Object : Program; Name : String)
     return Uniform_Sampler is
      Sampler_Kind : constant LE.Resource_Type := Object.GL_Program.Uniform_Type (Name);
      Kind_Image : String renames LE.Resource_Type'Image (Sampler_Kind);
   begin
      for Texture_Kind in Sampler_Kind_Array'Range loop
         if Sampler_Kind_Array (Texture_Kind) = Sampler_Kind then
            return Uniform_Sampler'
              (Kind       => Texture_Kind,
               GL_Uniform => Object.GL_Program.Uniform_Location (Name));
         end if;
      end loop;
      raise Uniform_Type_Error with "Uniform " & Name & " has unexpected sampler type " & Kind_Image;
   end Create_Uniform_Sampler;

   function Create_Uniform_Variable (Object : Program; Name : String)
     return Uniform is
      Uniform_Kind : constant LE.Resource_Type := Object.GL_Program.Uniform_Type (Name);
   begin
      return Uniform'
        (Kind       => Uniform_Kind,
         GL_Uniform => Object.GL_Program.Uniform_Location (Name));
   end Create_Uniform_Variable;

end Orka.Programs.Uniforms;
